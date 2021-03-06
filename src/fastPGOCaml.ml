(* PG'OCaml is a set of OCaml bindings for the PostgreSQL database.
 * $Id: pGOCaml.ml,v 1.21 2007/02/24 17:27:24 rich Exp $
 *)

open CalendarLib
open Printf

type 'a t = {
  ichan : in_channel;			(* In_channel wrapping socket. *)
  chan : out_channel;			(* Out_channel wrapping socket. *)
  mutable private_data : 'a option;
  uuid : string;			(* UUID for this connection. *)
}

(* FT-Patch - inizio *)

type 'a result_set = {
  rs_connection : 'a t;
  rs_current : string option array;
  rs_get : (unit -> string option);
  mutable rs_closed : bool
}

exception Ready_for_query

(* FT-Patch - fine *)

exception Error of string
exception Invalid_string
exception No_value

exception PostgreSQL_Error of string * (char * string) list

(* If true, emit a lot of debugging information about the protocol on stderr.*)
let debug_protocol = false

(*----- Code to generate messages for the back-end. -----*)

let new_message typ =
  let buf = Buffer.create 128 in
  buf, Some typ

(* StartUpMessage and SSLRequest are special messages which don't
 * have a type byte field.
 *)
let new_start_message () =
  let buf = Buffer.create 128 in
  buf, None

let add_byte (buf, _) i =
  (* Deliberately throw an exception if i isn't [0..255]. *)
  Buffer.add_char buf (Char.chr i)

let add_char (buf, _) c =
  Buffer.add_char buf c

let add_int16 (buf, _) i =
  if i < 0 || i > 65_535 then
    raise (Error "PGOCaml: int16 is outside range [0..65535].");
  Buffer.add_char buf (Char.unsafe_chr ((i lsr 8) land 0xff));
  Buffer.add_char buf (Char.unsafe_chr (i land 0xff))

let add_int32 (buf, _) i =
  let base = Int32.to_int i in
  let big = Int32.to_int (Int32.shift_right_logical i 24) in
  Buffer.add_char buf (Char.unsafe_chr (big land 0xff));
  Buffer.add_char buf (Char.unsafe_chr ((base lsr 16) land 0xff));
  Buffer.add_char buf (Char.unsafe_chr ((base lsr 8) land 0xff));
  Buffer.add_char buf (Char.unsafe_chr (base land 0xff))

let add_int64 msg i =
  add_int32 msg (Int64.to_int32 (Int64.shift_right_logical i 32));
  add_int32 msg (Int64.to_int32 i)

let add_string_no_trailing_nil (buf, _) str =
  (* Check the string doesn't contain '\0' characters. *)
  if String.contains str '\000' then
    raise (Error (sprintf "PGOCaml: string contains ASCII NIL character: %S" str));
  if String.length str > 0x3fff_ffff then
    raise (Error "PGOCaml: string is too long.");
  Buffer.add_string buf str

let add_string msg str =
  add_string_no_trailing_nil msg str;
  add_byte msg 0

let send_message { chan = chan } (buf, typ) =
  (* Get the length in bytes. *)
  let len = 4 + Buffer.length buf in

  (* If the length is longer than a 31 bit integer, then the message is
   * too long to send.  This limits messages to 1 GB, which should be
   * enough for anyone :-)
   *)
  if Int64.of_int len >= 0x4000_0000L then
    raise (Error "PGOCaml: message is larger than 1 GB");

  if debug_protocol then
    eprintf "> %s%d %S\n%!"
      (match typ with
       | None -> ""
       | Some c -> sprintf "%c " c)
      len (Buffer.contents buf);

  (* Write the type byte? *)
  (match typ with
   | None -> ()
   | Some c -> output_char chan c
  );

  (* Write the length field. *)
  output_binary_int chan len;

  (* Write the buffer. *)
  Buffer.output_buffer chan buf

(* Max message length accepted from back-end. *)
let max_message_length = ref Sys.max_string_length

(* Receive a single result message.  Parse out the message type,
 * message length, and binary message content.
 *)
let receive_message { ichan = ichan; chan = chan } =
  (* Flush output buffer. *)
  flush chan;

  let typ = input_char ichan in
  let len = input_binary_int ichan in

  (* Discount the length word itself. *)
  let len = len - 4 in

  (* If the message is too long, give up now. *)
  if len > !max_message_length then (
    (* Skip the message so we stay in synch with the stream. *)
    let bufsize = 65_536 in
    let buf = String.create bufsize in
    let n = ref len in
    while !n > 0 do
      let m = min !n bufsize in
      really_input ichan buf 0 m;
      n := !n - m
    done;

    raise (Error
	     "PGOCaml: back-end message is longer than max_message_length")
  );

  (* Read the binary message content. *)
  let msg = String.create len in
  really_input ichan msg 0 len;
  typ, msg

(* Send a message and expect a single result. *)
let send_recv conn msg =
  send_message conn msg;
  receive_message conn

(* Parse a back-end message. *)
type msg_t =
  | AuthenticationOk
  | AuthenticationKerberosV5
  | AuthenticationCleartextPassword
  | AuthenticationCryptPassword of string
  | AuthenticationMD5Password of string
  | AuthenticationSCMCredential
  | BackendKeyData of int32 * int32
  | BindComplete
  | CloseComplete
  | CommandComplete of string
(*  | DataRow of (int * string) list*)
  | DataRow of string option array
  | DataRowIgnore
  | EmptyQueryResponse
  | ErrorResponse of (char * string) list
  | NoData
  | NoticeResponse of (char * string) list
  | ParameterDescription of int32 list
  | ParameterStatus of string * string
  | ParseComplete
  | ReadyForQuery of char
  | RowDescription of (string * int32 * int * int32 * int * int32 * int) list
  | UnknownMessage of char * string

let string_of_msg_t = function
  | AuthenticationOk -> "AuthenticationOk"
  | AuthenticationKerberosV5 -> "AuthenticationKerberosV5"
  | AuthenticationCleartextPassword -> "AuthenticationCleartextPassword"
  | AuthenticationCryptPassword str ->
      sprintf "AuthenticationCleartextPassword %S" str
  | AuthenticationMD5Password str ->
      sprintf "AuthenticationMD5Password %S" str
  | AuthenticationSCMCredential -> "AuthenticationMD5Password"
  | BackendKeyData (i1, i2) ->
      sprintf "BackendKeyData %ld, %ld" i1 i2
  | BindComplete -> "BindComplete"
  | CloseComplete -> "CloseComplete"
  | CommandComplete str ->
      sprintf "CommandComplete %S" str
(*  | DataRow fields ->
      sprintf "DataRow [%s]"
	(String.concat "; "
	   (List.map (fun (len, bytes) -> sprintf "%d, %S" len bytes) fields))
*)  | DataRow fields ->
      sprintf "DataRow [%s]"
	(String.concat "; "
	   (List.map (fun x ->
             let len, bytes = match x with None -> 0, "NULL" | Some bytes -> (String.length bytes), bytes in
             sprintf "%d, %S" len bytes) (Array.to_list fields)))
  | DataRowIgnore -> "DataRowIgnore"
  | EmptyQueryResponse -> "EmptyQueryResponse"
  | ErrorResponse strs ->
      sprintf "ErrorResponse [%s]"
	(String.concat "; "
	   (List.map (fun (k, v) -> sprintf "%c, %S" k v) strs))
  | NoData -> "NoData"
  | NoticeResponse strs ->
      sprintf "NoticeResponse [%s]"
	(String.concat "; "
	   (List.map (fun (k, v) -> sprintf "%c, %S" k v) strs))
  | ParameterDescription fields ->
      sprintf "ParameterDescription [%s]"
	(String.concat "; "
	   (List.map (fun oid -> sprintf "%ld" oid) fields))
  | ParameterStatus (s1, s2) ->
      sprintf "ParameterStatus %S, %S" s1 s2
  | ParseComplete -> "ParseComplete"
  | ReadyForQuery c ->
      sprintf "ReadyForQuery %s"
	(match c with
	 | 'I' -> "Idle"
	 | 'T' -> "inTransaction"
	 | 'E' -> "Error"
	 | c -> sprintf "unknown(%c)" c)
  | RowDescription fields ->
      sprintf "RowDescription [%s]"
	(String.concat "; "
	   (List.map (fun (name, table, col, oid, len, modifier, format) ->
			sprintf "%s %ld %d %ld %d %ld %d"
			  name table col oid len modifier format) fields))
  | UnknownMessage (typ, msg) ->
      sprintf "UnknownMessage %c, %S" typ msg

let parse_backend_message ?(ignore_data=false) (typ, msg) =
  let pos = ref 0 in
  let len = String.length msg in

  (* Functions to grab the next object from the string 'msg'. *)
  let get_char where =
    if !pos < len then (
      let r = msg.[!pos] in
      incr pos;
      r
    ) else
      raise (Error ("PGOCaml: parse_backend_message: " ^ where ^
		    ": short message"))
  in
  let get_byte where = Char.code (get_char where) in
  let get_int16 () =
    let r0 = get_byte "get_int16" in
    let r1 = get_byte "get_int16" in
    (r0 lsr 8) + r1
  in
  let get_int32 () =
    let r0 = get_byte "get_int32" in
    let r1 = get_byte "get_int32" in
    let r2 = get_byte "get_int32" in
    let r3 = get_byte "get_int32" in
    let r = Int32.of_int r0 in
    let r = Int32.shift_left r 8 in
    let r = Int32.logor r (Int32.of_int r1) in
    let r = Int32.shift_left r 8 in
    let r = Int32.logor r (Int32.of_int r2) in
    let r = Int32.shift_left r 8 in
    let r = Int32.logor r (Int32.of_int r3) in
    r
  in
  (*let get_int64 () =
    let r0 = get_byte "get_int64" in
    let r1 = get_byte "get_int64" in
    let r2 = get_byte "get_int64" in
    let r3 = get_byte "get_int64" in
    let r4 = get_byte "get_int64" in
    let r5 = get_byte "get_int64" in
    let r6 = get_byte "get_int64" in
    let r7 = get_byte "get_int64" in
    let r = Int64.of_int r0 in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r1) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r2) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r3) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r4) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r5) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r6) in
    let r = Int64.shift_left r 8 in
    let r = Int64.logor r (Int64.of_int r7) in
    r
  in*)
  let get_string () =
    let buf = Buffer.create 16 in
    let rec loop () =
      let c = get_char "get_string" in
      if c <> '\000' then (
	Buffer.add_char buf c;
	loop ()
      ) else
	Buffer.contents buf
    in
    loop ()
  in
  let get_n_bytes n =
    let str = String.create n in
    for i = 0 to n-1 do
      str.[i] <- get_char "get_n_bytes"
    done;
    str
  in
  let get_char () = get_char "get_char" in
  (*let get_byte () = get_byte "get_byte" in*)

  let msg =
    match typ with
    | 'R' ->
	let t = get_int32 () in
	(match t with
	 | 0l -> AuthenticationOk
	 | 2l -> AuthenticationKerberosV5
	 | 3l -> AuthenticationCleartextPassword
	 | 4l ->
	     let salt = String.create 2 in
	     for i = 0 to 2 do
	       salt.[i] <- get_char ()
	     done;
	     AuthenticationCryptPassword salt
	 | 5l ->
	     let salt = String.create 4 in
	     for i = 0 to 3 do
	       salt.[i] <- get_char ()
	     done;
	     AuthenticationMD5Password salt
	 | 6l -> AuthenticationSCMCredential
	 | _ -> UnknownMessage (typ, msg)
	);

    | 'E' ->
	let strs = ref [] in
	let rec loop () =
	  let field_type = get_char () in
	  if field_type = '\000' then List.rev !strs (* end of list *)
	  else (
	    strs := (field_type, get_string ()) :: !strs;
	    loop ()
	  )
	in
	ErrorResponse (loop ())

    | 'N' ->
	let strs = ref [] in
	let rec loop () =
	  let field_type = get_char () in
	  if field_type = '\000' then List.rev !strs (* end of list *)
	  else (
	    strs := (field_type, get_string ()) :: !strs;
	    loop ()
	  )
	in
	NoticeResponse (loop ())

    | 'Z' ->
	let c = get_char () in
	ReadyForQuery c

    | 'K' ->
	let pid = get_int32 () in
	let key = get_int32 () in
	BackendKeyData (pid, key)

    | 'S' ->
	let param = get_string () in
	let value = get_string () in
	ParameterStatus (param, value)

    | '1' -> ParseComplete

    | '2' -> BindComplete

    | '3' -> CloseComplete

    | 'C' ->
	let str = get_string () in
	CommandComplete str

    | 'D' ->
      (* FT-Patch - inizio *)
(*	let nr_fields = get_int16 () in
	let fields = ref [] in
	for i = 0 to nr_fields-1 do
	  let len = get_int32 () in
	  let field =
	    if len < 0l then (-1, "")
	    else (
	      if len >= 0x4000_0000l then
		raise (Error "PGOCaml: result field is too long");
	      let len = Int32.to_int len in
	      if len > Sys.max_string_length then
		raise (Error "PGOCaml: result field is too wide for string");
	      let bytes = get_n_bytes len in
	      len, bytes
	    ) in
	  fields := field :: !fields
	done;
	DataRow (List.rev !fields)
*)
        if ignore_data then DataRowIgnore else
  	let nr_fields = get_int16 () in
  	let fields = Array.create nr_fields None in
  	for i = 0 to nr_fields - 1 do
  	  let len = get_int32 () in
            if len >= 0l then begin
              if len >= 0x4000_0000l then raise (Error "FastPGOCaml: result field is too long");
              let len = Int32.to_int len in
              if len > Sys.max_string_length then raise (Error "FastPGOCaml: result field is too wide for string");
              if len = 0 then fields.(i) <- Some "" else begin
                (*fields.(i) <- Some (get_n_bytes len)*)
                Array.unsafe_set fields i (Some (get_n_bytes len))
              end
            end
  	done;
  	DataRow fields
      (* FT-Patch - fine *)

    | 'I' -> EmptyQueryResponse

    | 'n' -> NoData

    | 'T' ->
	let nr_fields = get_int16 () in
	let fields = ref [] in
	for i = 0 to nr_fields-1 do
	  let name = get_string () in
	  let table = get_int32 () in
	  let column = get_int16 () in
	  let oid = get_int32 () in
	  let length = get_int16 () in
	  let modifier = get_int32 () in
	  let format = get_int16 () in
	  fields := (name, table, column, oid, length, modifier, format)
	    :: !fields
	done;
	RowDescription (List.rev !fields)

    | 't' ->
	let nr_fields = get_int16 () in
	let fields = ref [] in
	for i = 0 to nr_fields - 1 do
	  let oid = get_int32 () in
	  fields := oid :: !fields
	done;
	ParameterDescription (List.rev !fields)

    | _ -> UnknownMessage (typ, msg) in

  if debug_protocol then eprintf "< %s\n%!" (string_of_msg_t msg);

  msg

let verbose = ref 1

(* Print an ErrorResponse on stderr. *)
let print_ErrorResponse fields =
  if !verbose >= 1 then (
    try
      let severity = List.assoc 'S' fields in
      let code = List.assoc 'C' fields in
      let message = List.assoc 'M' fields in
      if !verbose = 1 then
	match severity with
	| "ERROR" | "FATAL" | "PANIC" ->
	    eprintf "%s: %s: %s\n%!" severity code message
	| _ -> ()
      else
	eprintf "%s: %s: %s\n%!" severity code message
    with
      Not_found ->
	eprintf
	  "WARNING: 'Always present' field is missing in error message\n%!"
  );
  if !verbose >= 2 then (
    List.iter (
      fun (field_type, field) ->
	if field_type <> 'S' && field_type <> 'C' && field_type <> 'M' then
	  eprintf "%c: %s\n%!" field_type field
    ) fields
  )

(* Handle an ErrorResponse anywhere, by printing and raising an exception. *)
let pg_error ?conn fields =
  print_ErrorResponse fields;
  let str =
    try
      let severity = List.assoc 'S' fields in
      let code = List.assoc 'C' fields in
      let message = List.assoc 'M' fields in
      sprintf "%s: %s: %s" severity code message
    with
      Not_found ->
	"WARNING: 'Always present' field is missing in error message" in

  (* If conn parameter was given, then resynch - read messages until we
   * see ReadyForQuery.
   *)
  (match conn with
   | None -> ()
   | Some conn ->
       let rec loop () =
	 let msg = receive_message conn in
	 let msg = parse_backend_message msg in
	 match msg with ReadyForQuery _ -> () | _ -> loop ()
       in
       loop ()
  );

  raise (PostgreSQL_Error (str, fields))


(*----- Connection. -----*)

let connect ?host ?port ?user ?(password = "") ?database
    ?(unix_domain_socket_dir = "/var/run/postgresql") () =
  (* Get the username. *)
  let user =
    match user with
    | Some user -> user
    | None ->
	try Sys.getenv "PGUSER"
	with Not_found ->
	  try
	    let pw = Unix.getpwuid (Unix.geteuid ()) in
	    pw.Unix.pw_name
	  with
	    Not_found -> "postgres" in

  (* Get the database name. *)
  let database =
    match database with
    | Some database -> database
    | None ->
	try Sys.getenv "PGDATABASE"
	with Not_found -> user in

  (* Hostname and port number. *)
  let host =
    match host with
    | Some _ -> host
    | None ->
	try Some (Sys.getenv "PGHOST")
	with Not_found -> None in (* use Unix domain socket. *)

  let port =
    match port with
    | Some port -> port
    | None ->
	try int_of_string (Sys.getenv "PGPORT")
	with Not_found | Failure "int_of_string" -> 5432 in

  (* Make the socket address. *)
  let sockaddr =
    match host with
    | Some hostname ->
	(try
	   let hostent = Unix.gethostbyname hostname in
	   let domain = hostent.Unix.h_addrtype in
	   match domain with
	   | Unix.PF_INET | Unix.PF_INET6 ->
	       (* Choose a random address from the list. *)
	       let addrs = hostent.Unix.h_addr_list in
	       let len = Array.length addrs in
	       if len <= 0 then
		 raise (Error ("PGOCaml: unknown host: " ^ hostname));
	       let i = Random.int len in
	       let addr = addrs.(i) in
	       Unix.ADDR_INET (addr, port)
	   | Unix.PF_UNIX ->
	       (* Would we trust a pathname returned through DNS? *)
	       raise (Error "PGOCaml: DNS returned PF_UNIX record")
	 with
	   Not_found ->
	     raise (Error ("PGOCaml: unknown host: " ^ hostname))
	);
    | None -> (* Unix domain socket. *)
	let sockaddr = sprintf "%s/.s.PGSQL.%d" unix_domain_socket_dir port in
	Unix.ADDR_UNIX sockaddr in

  (* Create a universally unique identifier for this connection.  This
   * is mainly for debugging and profiling.
   *)
  let uuid =
    sprintf "%s %d %g %s %g"
      (Unix.gethostname ())
      (Unix.getpid ())
      (*(Unix.getppid ())*)
      (Unix.gettimeofday ())
      Sys.executable_name
      ((Unix.times ()).Unix.tms_utime) in
  let uuid = Digest.to_hex (Digest.string uuid) in

  let ichan, chan = Unix.open_connection sockaddr in

  (* Create the connection structure. *)
  let conn = { ichan = ichan;
 chan = chan;
 private_data = None;
 uuid = uuid } in

  (* Send the StartUpMessage.  NB. At present we do not support SSL. *)
  let msg = new_start_message () in
  add_int32 msg 196608l;
  add_string msg "user"; add_string msg user;
  add_string msg "database"; add_string msg database;
  add_byte msg 0;

  (* Loop around here until the database gives a ReadyForQuery message. *)
  let rec loop msg =
    let msg =
match msg with
| Some msg -> send_recv conn msg
| None -> receive_message conn in
    let msg = parse_backend_message msg in

    match msg with
    | ReadyForQuery _ -> () (* Finished connecting! *)
    | BackendKeyData _ ->
 (* XXX We should save this key. *)
 loop None
    | ParameterStatus _ ->
 (* Should we do something with this? *)
 loop None
    | AuthenticationOk -> loop None
    | AuthenticationKerberosV5 ->
 raise (Error "PGOCaml: Kerberos authentication not supported")
    | AuthenticationCleartextPassword ->
 let msg = new_message 'p' in (* PasswordMessage *)
 add_string msg password;
 loop (Some msg)
    | AuthenticationCryptPassword salt ->
 (* Crypt password not supported because there is no crypt(3) function
  * in OCaml.
  *)
 raise (Error "PGOCaml: crypt password authentication not supported")
    | AuthenticationMD5Password salt ->
 (*	(* This is a guess at how the salt is used ... *)
let password = salt ^ password in
let password = Digest.string password in*)
 let password = "md5" ^ Digest.to_hex (Digest.string (Digest.to_hex (Digest.string (password ^ user)) ^ salt)) in
 let msg = new_message 'p' in (* PasswordMessage *)
 add_string msg password;
 loop (Some msg)
    | AuthenticationSCMCredential ->
 raise (Error "PGOCaml: SCM Credential authentication not supported")
    | ErrorResponse err ->
 pg_error ~conn err
    | NoticeResponse err ->
 (* XXX Do or print something here? *)
 loop None
    | _ ->
 (* Silently ignore unknown or unexpected message types. *)
 loop None
  in
  loop (Some msg);

  conn

let close conn =
  (* Be nice and send the terminate message. *)
  let msg = new_message 'X' in
(*  begin
    try*)
      send_message conn msg;
      flush conn.chan;
    (*with (Failure("output_binary_int: not a binary channel")) as ex ->
      Printf.eprintf "File \"fastPGOCaml.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
  end;*)
  (* Closes the underlying socket too. *)
  close_in conn.ichan

let set_private_data conn data =
  conn.private_data <- Some data

let private_data { private_data = private_data } =
  match private_data with
  | None -> raise Not_found
  | Some private_data -> private_data

type pa_pg_data = (string, bool) Hashtbl.t

let ping conn =
  let msg = new_message 'S' in
  send_message conn msg;

  (* Wait for ReadyForQuery. *)
  let rec loop () =
    let msg = receive_message conn in
    let msg = parse_backend_message msg in
    match msg with
    | ReadyForQuery _ -> () (* Finished! *)
    | ErrorResponse err -> pg_error ~conn err (* Error *)
    | _ -> loop ()
  in
  loop()

type oid = int32

type param = string option
type result = string option
type row = result list

let flush_msg conn =
  let msg = new_message 'H' in
  send_message conn msg;
  (* Might as well actually flush the channel too, otherwise what is the
   * point of executing this command?
   *)
  flush conn.chan

let prepare conn ~query ?(name = "") ?(types = []) () =
  let msg = new_message 'P' in
  add_string msg name;
  add_string msg query;
  add_int16 msg (List.length types);
  List.iter (add_int32 msg) types;
  send_message conn msg;
  flush_msg conn;
  let rec loop () =
    let msg = receive_message conn in
    let msg = parse_backend_message msg in
    match msg with
    | ErrorResponse err -> pg_error err
    | ParseComplete -> () (* Finished! *)
    | NoticeResponse _ ->
 (* XXX Do or print something here? *)
 loop ()
    | _ ->
 raise (Error ("PGOCaml: unknown response from parse: " ^
	  string_of_msg_t msg))
  in
  loop ()

let execute conn ?(name = "") ?(portal = "") ~params () =
  (* Bind *)
  let msg = new_message 'B' in
  add_string msg portal;
  add_string msg name;
  add_int16 msg 0; (* Send all parameters as text. *)
  add_int16 msg (List.length params);
  List.iter (
    fun param ->
      match param with
      | None -> add_int32 msg 0xffff_ffffl (* NULL *)
      | Some str ->
         add_int32 msg (Int32.of_int (String.length str));
         add_string_no_trailing_nil msg str
  ) params;
  add_int16 msg 0; (* Send back all results as text. *)
  send_message conn msg;

  (* Execute *)
  let msg = new_message 'E' in
  add_string msg portal;
  add_int32 msg 0l; (* no limit on rows *)
  send_message conn msg;

  (* Sync *)
  let msg = new_message 'S' in
  send_message conn msg;



(* FT-Patch - inizio *)

  {
    rs_connection = conn;
    rs_current = [||];
    rs_get = (function () -> assert false);
    rs_closed = false;
  }

(*  (* Process the message(s) received from the database until we read
   * ReadyForQuery.  In the process we may get some rows back from
   * the database, no data, or an error.
   *)
  let rows = ref [] in
  let rec loop () =
    (* NB: receive_message flushes the output connection. *)
    let msg = receive_message conn in
    let msg = parse_backend_message msg in
    match msg with
    | ReadyForQuery _ -> () (* Finished! *)
    | ErrorResponse err -> pg_error ~conn err (* Error *)
    | NoticeResponse err ->
 (* XXX Do or print something here? *)
 loop ()
    | BindComplete -> loop ()
    | CommandComplete _ -> loop ()
    | EmptyQueryResponse -> loop ()
    | DataRow fields ->
 let fields = List.map (
   function
   | (i, _) when i < 0 -> None (* NULL *)
   | (0, _) -> Some ""
   | (i, bytes) -> Some bytes
 ) fields in
 rows := fields :: !rows;
 loop ()
    | NoData -> loop ()
    | ParameterStatus _ ->
 (* 43.2.6: ParameterStatus messages will be generated whenever
  * the active value changes for any of the parameters the backend
  * believes the frontend should know about. Most commonly this
  * occurs in response to a SET SQL command executed by the
  * frontend, and this case is effectively synchronous -- but it
  * is also possible for parameter status changes to occur because
  * the administrator changed a configuration file and then sent
  * the SIGHUP signal to the postmaster.
  *)
 loop ()
    | _ ->
 raise
   (Error ("PGOCaml: unknown response message: " ^
      string_of_msg_t msg))
  in
  loop ();

  (* Return the result rows. *)
  List.rev !rows
*)


let rec next rs =
  let conn = rs.rs_connection in
  (* NB: receive_message flushes the output connection. *)
  let msg = receive_message conn in
  let msg = parse_backend_message msg in
  match msg with
    | ReadyForQuery _ ->
      rs.rs_closed <- true;
      raise Ready_for_query (* Finished! *)
    | ErrorResponse err -> pg_error ~conn err (* Error *)
    | NoticeResponse err -> (* XXX Do or print something here? *) next rs
    | BindComplete -> next rs
    | CommandComplete _ -> next rs
    | EmptyQueryResponse -> next rs
    | DataRow fields -> fields
    | NoData -> next rs
    | ParameterStatus _ ->
     (* 43.2.6: ParameterStatus messages will be generated whenever
      * the active value changes for any of the parameters the backend
      * believes the frontend should know about. Most commonly this
      * occurs in response to a SET SQL command executed by the
      * frontend, and this case is effectively synchronous -- but it
      * is also possible for parameter status changes to occur because
      * the administrator changed a configuration file and then sent
      * the SIGHUP signal to the postmaster.
      *)
      next rs
    | _ -> raise (Error ("FastPGOCaml: unknown response message: " ^ string_of_msg_t msg))

let rec finish conn =
  (* NB: receive_message flushes the output connection. *)
  let msg = receive_message conn in
  let msg = parse_backend_message ~ignore_data:true msg in
  match msg with
    | ReadyForQuery _ -> () (* Finished! *)
    | ErrorResponse err -> pg_error ~conn err (* Error *)
    | NoticeResponse err -> (* XXX Do or print something here? *) finish conn
    | BindComplete -> finish conn
    | CommandComplete _ -> finish conn
    | EmptyQueryResponse -> finish conn
    | DataRow _ -> finish conn
    | DataRowIgnore -> finish conn
    | NoData -> finish conn
    | ParameterStatus _ ->
     (* 43.2.6: ParameterStatus messages will be generated whenever
      * the active value changes for any of the parameters the backend
      * believes the frontend should know about. Most commonly this
      * occurs in response to a SET SQL command executed by the
      * frontend, and this case is effectively synchronous -- but it
      * is also possible for parameter status changes to occur because
      * the administrator changed a configuration file and then sent
      * the SIGHUP signal to the postmaster.
      *)
      finish conn
    | _ ->  raise (Error ("FastPGOCaml: unknown response message: " ^ string_of_msg_t msg))

let close_result_set rs =
  if not rs.rs_closed then begin
    let conn = rs.rs_connection in
    finish conn;
    rs.rs_closed <- true
  end

let execute_update db ?name ?portal ~params () =
  let rs = execute db ?name ?portal ~params () in
  try ignore (next rs) with Ready_for_query -> ()

(* FT-Patch - fine *)

let begin_work conn =
  let query = "begin work" in
  prepare conn ~query ();
  ignore (execute_update conn ~params:[] ())

let commit conn =
  let query = "commit" in
  prepare conn ~query ();
  ignore (execute_update conn ~params:[] ())

let rollback conn =
  let query = "rollback" in
  prepare conn ~query ();
  ignore (execute_update conn ~params:[] ())

let serial conn name =
  let query = "select currval ($1)" in
  prepare conn ~query ();
  (* FT-Patch - inizio *)
(*  let rows = execute conn ~params:[Some name] () in
  let row = List.hd rows in
  let result = List.hd row in*)
  let rows = execute conn ~params:[Some name] () in
  let result = (next rows).(0) in
  (* FT-Patch - fine *)

  (* NB. According to the manual, the return type of currval is
   * always a bigint, whether or not the column is serial or bigserial.
   *)
  Int64.of_string (match result with None -> raise No_value | Some v -> v)

let serial4 conn name =
  Int64.to_int32 (serial conn name)

let serial8 = serial

let close_statement conn ?(name = "") () =
  let msg = new_message 'C' in
  add_char msg 'S';
  add_string msg name;
  send_message conn msg;
  flush_msg conn;
  let rec loop () =
    let msg = receive_message conn in
    let msg = parse_backend_message msg in
    match msg with
    | ErrorResponse err ->
      pg_error err
    | CloseComplete -> () (* Finished! *)
    | NoticeResponse _ ->
	(* XXX Do or print something here? *)
	loop ()
    | _ ->
	raise (Error ("PGOCaml: unknown response from close: " ^
			string_of_msg_t msg))
  in
  loop ()

let close_portal conn ?(portal = "") () =
  let msg = new_message 'C' in
  add_char msg 'P';
  add_string msg portal;
  send_message conn msg;
  flush_msg conn;
  let rec loop () =
    let msg = receive_message conn in
    let msg = parse_backend_message msg in
    match msg with
    | ErrorResponse err -> pg_error err
    | CloseComplete -> ()
    | NoticeResponse _ ->
	(* XXX Do or print something here? *)
	loop ()
    | _ ->
	raise (Error ("PGOCaml: unknown response from close: " ^
			string_of_msg_t msg))
  in
  loop ()

type row_description = result_description list
and result_description = {
  name : string;
  table : oid option;
  column : int option;
  field_type : oid;
  length : int;
  modifier : int32;
}

type params_description = param_description list
and param_description = {
  param_type : oid;
}

let describe_statement conn ?(name = "") () =
  let msg = new_message 'D' in
  add_char msg 'S';
  add_string msg name;
  send_message conn msg;
  flush_msg conn;
  let msg = receive_message conn in
  let msg = parse_backend_message msg in
  let params =
    match msg with
    | ErrorResponse err -> pg_error err
    | ParameterDescription params ->
	let params = List.map (
	  fun oid ->
	    { param_type = oid }
	) params in
	params
    | _ ->
	raise (Error ("PGOCaml: unknown response from describe: " ^
			string_of_msg_t msg)) in
  let msg = receive_message conn in
  let msg = parse_backend_message msg in
  match msg with
  | ErrorResponse err -> pg_error err
  | NoData -> params, None
  | RowDescription fields ->
      let fields = List.map (
	fun (name, table, column, oid, length, modifier, _) ->
	  {
	    name = name;
	    table = if table = 0l then None else Some table;
	    column = if column = 0 then None else Some column;
	    field_type = oid;
	    length = length;
	    modifier = modifier;
	  }
      ) fields in
      params, Some fields
  | _ ->
      raise (Error ("PGOCaml: unknown response from describe: " ^
		      string_of_msg_t msg))

let describe_portal conn ?(portal = "") () =
  let msg = new_message 'D' in
  add_char msg 'P';
  add_string msg portal;
  send_message conn msg;
  flush_msg conn;
  let msg = receive_message conn in
  let msg = parse_backend_message msg in
  match msg with
  | ErrorResponse err -> pg_error err
  | NoData -> None
  | RowDescription fields ->
      let fields = List.map (
	fun (name, table, column, oid, length, modifier, _) ->
	  {
	    name = name;
	    table = if table = 0l then None else Some table;
	    column = if column = 0 then None else Some column;
	    field_type = oid;
	    length = length;
	    modifier = modifier;
	  }
      ) fields in
      Some fields
  | _ ->
      raise (Error ("PGOCaml: unknown response from describe: " ^
		      string_of_msg_t msg))

(*----- Type conversion. -----*)

(* For certain types, more information is available by looking
 * at the modifier field as well as just the OID.  For example,
 * for NUMERIC the modifier tells us the precision.
 * However we don't always have the modifier field available -
 * in particular for parameters.
 *)
let name_of_type ?modifier = function
  | 16_l -> "bool"			(* BOOLEAN *)
  | 17_l -> "bytea"			(* BYTEA *)
  | 20_l -> "int64"			(* INT8 *)
  | 21_l -> "int16"			(* INT2 *)
  | 23_l -> "int32"			(* INT4 *)
  | 25_l -> "string"			(* TEXT *)
  | 700_l | 701_l -> "float"		(* FLOAT4, FLOAT8 *)
  | 1007_l -> "int32_array"		(* INT4[] *)
  | 1042_l -> "string"			(* CHAR(n) - treat as string *)
  | 1082_l -> "date"			(* DATE *)
  | 1083_l -> "time"			(* TIME *)
  | 1114_l -> "timestamp"		(* TIMESTAMP *)
  | 1184_l -> "timestamptz"             (* TIMESTAMP WITH TIME ZONE *)
  | 1186_l -> "interval"		(* INTERVAL *)
  | 1700_l ->
      (* XXX This is wrong - it will be changed to a fixed precision
       * numeric type later.
       *)
      (match modifier with
       | None -> "float"
       | Some modifier when modifier = -1_l -> "float"
       | Some modifier ->
	   (* XXX *)
	   eprintf "numeric modifier = %ld\n%!" modifier;
	   "float"
      );
  | i ->
      (* For unknown types, look at <postgresql/catalog/pg_type.h>. *)
      raise (Error ("PGOCaml: unknown type for OID " ^ Int32.to_string i))

type timestamptz = Calendar.t * Time_Zone.t

type int16 = int
type bytea = string

type int32_array = int32 array

let string_of_oid = Int32.to_string
let string_of_bool = function
  | true -> "t"
  | false -> "f"
let string_of_int = Pervasives.string_of_int
let string_of_int16 = Pervasives.string_of_int
let string_of_int32 = Int32.to_string
let string_of_int64 = Int64.to_string
let string_of_float = string_of_float
let string_of_timestamp = Printer.CalendarPrinter.to_string
let string_of_timestamptz (cal, tz) =
  Printer.CalendarPrinter.to_string cal ^
    match tz with
    | Time_Zone.UTC -> "+00"
    | Time_Zone.Local ->
	let gap = Time_Zone.gap Time_Zone.UTC Time_Zone.Local in
	if gap >= 0 then sprintf "+%02d" gap
	else sprintf "-%02d" (-gap)
    | Time_Zone.UTC_Plus gap ->
	if gap >= 0 then sprintf "+%02d" gap
	else sprintf "-%02d" (-gap)
let string_of_date = Printer.DatePrinter.to_string
let string_of_time = Printer.TimePrinter.to_string
let string_of_interval p =
  let y, m, d, s = Calendar.Period.ymds p in
  sprintf "%d years %d mons %d days %d seconds" y m d s

(* NB. It is the responsibility of the caller of this function to
 * properly escape array elements.
 *)
let string_of_any_array a =
  let buf = Buffer.create 128 in
  Buffer.add_char buf '{';
  for i = 0 to Array.length a - 1 do
    if i > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf a.(i);
  done;
  Buffer.add_char buf '}';
  Buffer.contents buf

let string_of_int32_array a =
  let a = Array.map Int32.to_string a in
  string_of_any_array a

let string_of_bytea b =
  let len = String.length b in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let c = b.[i] in
    let cc = Char.code c in
    if cc < 0x20 || cc > 0x7e then
      Buffer.add_string buf (sprintf "\\%03o" cc) (* non-print -> \ooo *)
    else if c = '\\' then
      Buffer.add_string buf "\\\\" (* \ -> \\ *)
    else
      Buffer.add_char buf c
  done;
  Buffer.contents buf

let string_of_string (x : string) = x

let bool_of_string = function
  | "true" | "t" -> true
  | "false" | "f" -> false
  | str ->
      raise (Error ("PGOCaml: not a boolean: " ^ str))
let int_of_string = Pervasives.int_of_string
let int16_of_string = Pervasives.int_of_string
let int32_of_string = Int32.of_string
let int64_of_string = Int64.of_string
let float_of_string = float_of_string
let date_of_string = Printer.DatePrinter.from_string

let time_of_string str =
  (* Remove trailing ".microsecs" if present. *)
  let n = String.length str in
  let str =
    if n > 8 && str.[8] = '.' then
      String.sub str 0 8
    else str in
  Printer.TimePrinter.from_string str

let timestamp_of_string str =
  (* Remove trailing ".microsecs" if present. *)
  let n = String.length str in
  let str =
    if n > 19 && str.[19] = '.' then
      String.sub str 0 19
    else str in
  Printer.CalendarPrinter.from_string str

let timestamptz_of_string str =
  (* Split into datetime+timestamp. *)
  let n = String.length str in
  let cal, tz =
    if n >= 3 && (str.[n-3] = '+' || str.[n-3] = '-') then
      String.sub str 0 (n-3), Some (String.sub str (n-3) 3)
    else
      str, None in
  let cal = timestamp_of_string cal in
  let tz = match tz with
    | None -> Time_Zone.Local (* best guess? *)
    | Some tz ->
	let sgn = match tz.[0] with '+' -> 1 | '-' -> 0 | _ -> assert false in
	let mag = int_of_string (String.sub tz 1 2) in
	Time_Zone.UTC_Plus (sgn * mag) in
  cal, tz


(* FT-Patch - inizio *)

(*let re_interval =
  Pcre.regexp ~flags:[`EXTENDED]
    ("(?:(\\d+)\\syears?)?                     # years\n"^
     "\\s*                                     # \n"^
     "(?:(\\d+)\\smons?)?                      # months\n"^
     "\\s*                                     # \n"^
     "(?:(\\d+)\\sdays?)?                      # days\n"^
     "\\s*                                     # \n"^
     "(?:(\\d\\d):(\\d\\d)                     # HH:MM\n"^
     "   (?::(\\d\\d))?                        # optional :SS\n"^
     ")?")

let interval_of_string =
  let int_opt s = if s = "" then 0 else int_of_string s in
  fun str ->
    try
      let sub = Pcre.extract ~rex:re_interval str in
      Calendar.Period.make
	(int_opt sub.(1)) (* year *)
        (int_opt sub.(2)) (* month *)
        (int_opt sub.(3)) (* day *)
	(int_opt sub.(4)) (* hour *)
        (int_opt sub.(5)) (* min *)
        (int_opt sub.(6)) (* sec *)
    with
      Not_found -> failwith ("interval_of_string: bad interval: " ^ str) *)

let re_interval = Str.regexp "\\(\\([0-9]+\\)[ \t]*years?\\)?[ \t]*\\(\\([0-9]+\\)[ \t]*mons?\\)?[ \t]*\\(\\([0-9]+\\)[ \t]*days?\\)?[ \t]*\\(\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(:\\([0-9][0-9]\\)\\)?\\)?"

let interval_of_string str =
  let int_opt g = try int_of_string (Str.matched_group g str) with Not_found -> 0 in
  if Str.string_match re_interval str 0 then
    Calendar.Period.make
      (int_opt 2) (* year *)
      (int_opt 4) (* month *)
      (int_opt 6) (* day *)
      (int_opt 8) (* hour *)
      (int_opt 9) (* min *)
      (int_opt 11) (* sec *)
  else failwith ("interval_of_string: bad interval: " ^ str)


let find str sub =
	let sublen = String.length sub in
	if sublen = 0 then
		0
	else
		let found = ref 0 in
		let len = String.length str in
		try
			for i = 0 to len - sublen do
				let j = ref 0 in
				while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
					incr j;
					if !j = sublen then begin found := i; raise Exit; end;
				done;
			done;
			raise Invalid_string
		with
			Exit -> !found
let split str sep =
	let p = find str sep in
	let len = String.length sep in
	let slen = String.length str in
	String.sub str 0 p, String.sub str (p + len) (slen - p - len)

let rec nsplit str sep =
	try
		let s1 , s2 = split str sep in
		s1 :: nsplit s2 sep
	with
		Invalid_string -> [str]

(* FT-Patch - fine *)


(* NB. It is the responsibility of the caller of this function to
 * properly unescape returned elements.
 *)
let any_array_of_string str =
  let n = String.length str in
  assert (str.[0] = '{');
  assert (str.[n-1] = '}');
  let str = String.sub str 1 (n-2) in
  let fields = nsplit str "," in
  Array.of_list fields

let int32_array_of_string str =
  let a = any_array_of_string str in
  Array.map Int32.of_string a

let is_first_oct_digit c = c >= '0' && c <= '3'
let is_oct_digit c = c >= '0' && c <= '7'
let oct_val c = Char.code c - 0x30

let bytea_of_string str =
  let len = String.length str in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = str.[!i] in
    if c = '\\' then (
      incr i;
      if !i < len && str.[!i] = '\\' then (
	Buffer.add_char buf '\\';
	incr i
      ) else if !i+2 < len &&
	is_first_oct_digit str.[!i] &&
	is_oct_digit str.[!i+1] &&
	is_oct_digit str.[!i+2] then (
	  let byte = oct_val str.[!i] in
	  incr i;
	  let byte = (byte lsl 3) + oct_val str.[!i] in
	  incr i;
	  let byte = (byte lsl 3) + oct_val str.[!i] in
	  incr i;
	  Buffer.add_char buf (Char.chr byte)
	)
    ) else (
      incr i;
      Buffer.add_char buf c
    )
  done;
  Buffer.contents buf
