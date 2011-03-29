module PGOCaml = FastPGOCaml

module Datasource = Datasource.Make (
  struct
    type connection = unit PGOCaml.t
    let connect ~host ~port ~user ~password ~database () =
      PGOCaml.connect ~host ~port ~user ~password ~database ()
    let disconnect = PGOCaml.close;
  end
)

(** Iterativa *)
let replace_all re subst str =
  let search = Str.search_backward re str in
  let result = ref [] in
  let pos = ref (String.length str) in
  let prec = ref !pos in
  begin
    try
      while true do
        decr pos;
        if !pos < 0 then raise Not_found;
        pos := search !pos;
        let matched = Str.matched_string str in
        let end_matched = !pos + (String.length matched) in
        if end_matched <= !prec then begin
          result := (subst str) :: (String.sub str end_matched (!prec - end_matched)) :: !result;
          prec := !pos;
        end
      done
    with Not_found -> result := (String.sub str 0 (!pos + 1)) :: !result;
  end;
  String.concat "" !result


let read_file filename =
  let ichan = open_in_bin filename in
  let finally () = close_in ichan in
  try 
    let length = in_channel_length ichan in
    let data = Buffer.create length in
    Buffer.add_channel data ichan length;
    finally ();
    Buffer.contents data;
  with ex -> (finally(); raise ex)


let memo ~f =
  let memo = Hashtbl.create 1 in
  fun ?(force=fun _ -> false) key ->
    try
      let data = Hashtbl.find memo key in
      if force data then begin
        Hashtbl.remove memo key;
        raise Not_found;
      end;
      data
    with Not_found ->
      let data = f key in
      Hashtbl.add memo key data;
      data


let escape_slow str =
  let result = ref [] in
  String.iter begin function
    | '\'' -> result := "\\\\047" :: !result
    | '\\' -> result := "\\\\134" :: !result (* Backslash SI *)
    | c ->
      let dec = Char.code c in
      let repl = if dec > 31 && dec < 127 then String.make 1 c
      else Printf.sprintf "\\\\%03o" dec in
      result := repl :: !result
  end str;
  String.concat "" (List.rev !result)

(** escape *) 
let escape =
  let find n = List.assoc n [0, "0"; 1, "1"; 2, "2"; 3, "3"; 4 , "4"; 5, "5"; 6, "6"; 7, "7"] in
  let rec octal n =
    if n < 8 then find n else begin
      let q = n / 8 in
      let r = n mod 8 in
      (octal q) ^ (octal r)
    end in
  let octpad n =
    let s = octal n in
    "\\\\" ^ (String.make (3 - String.length s) '0') ^ s in
  let escape_char = function
    | '\'' -> "\\\\047" 
    | '\\' -> "\\\\134"
    | c ->
      let dec = Char.code c in
      if dec > 31 && dec < 127 then String.make 1 c else octpad dec 
  in
  let escape_char = memo ~f:escape_char in
  fun str ->
    let len = String.length str in
    let result = Buffer.create len in
    for i = 0 to len - 1 do
      Buffer.add_string result (escape_char (String.unsafe_get str i));
    done;
    Buffer.contents result

(** escape_chan *)
let escape_chan ?(bufsize=4096) inchan =
  let rbuf = Buffer.create bufsize in
  let result = ref [] in
  let buf = String.create bufsize in
  let add str length =
    let str = String.sub str 0 length in
    Buffer.add_string rbuf (escape str);
  in
  begin
    try
      while true do
        let length = input inchan buf 0 bufsize in
        if length = 0 then raise End_of_file else begin
          try add buf length
          with Failure "Buffer.add: cannot grow buffer" -> begin
            result := Buffer.contents rbuf :: !result;
            Printf.printf "==> length = %d\n%!" (Buffer.length rbuf);
            Buffer.reset rbuf;
            add buf length
          end
        end
      done;
    with End_of_file -> (result := Buffer.contents rbuf :: !result)
  end;
  List.rev !result

(** unescape *)
let unescape =
  let char (c1, c2, c3) =
    let code = String.make 5 '0' in
    String.unsafe_set code 1 'o';
    String.unsafe_set code 2 c1;
    String.unsafe_set code 3 c2;
    String.unsafe_set code 4 c3;
    Char.chr (int_of_string code)
  in
  let char = memo ~f:char in
  let apo, bs = '\'', '\\' in
  fun str ->
    let buf = Buffer.create (String.length str / 5) in
    let i = ref 0 in
    while !i < String.length str do
      (match String.unsafe_get str !i with
        | '\\' ->
          let next1 = incr i; String.unsafe_get str !i in
          if next1 = apo then Buffer.add_char buf apo
          else begin (* next1 deve essere '\\' *)
            let next2 = incr i; String.unsafe_get str !i in
            let next3 = incr i; String.unsafe_get str !i in
            if next2 = bs && next3 = bs then
              Buffer.add_char buf bs
            else begin
              let next4 = incr i; String.unsafe_get str !i in
              Buffer.add_char buf (char (next2, next3, next4))
            end
          end
        | c -> Buffer.add_char buf c);
      incr i;
    done;
    Buffer.contents buf


module SimpleQuery (*: Datasource.SIMPLE_QUERY*) =
  struct

    let query ~datasource ?name ~sql ?(params=[]) callback =
      (*Printf.printf "%s\n%!" query;*)
      let con = Datasource.get_connection datasource in
      let finally () =
        PGOCaml.close_statement ?name con ();
        Datasource.release_connection datasource con;
      in
      try
        let stmt = PGOCaml.prepare con ?name ~query:sql () in
        let rs = PGOCaml.execute con ?name ~params () in
        begin
          try
            while true do callback (PGOCaml.next rs) done
          with PGOCaml.Ready_for_query -> ()
        end;
        finally()
      with ex -> (finally(); raise ex)

    let query_first ~datasource ?name ~sql ?(params=[]) () =
      let con = Datasource.get_connection datasource in
      let finally () =
        PGOCaml.close_statement ?name con ();
        Datasource.release_connection datasource con;
      in
      try
        let stmt = PGOCaml.prepare con ?name ~query:sql () in
        let rs = PGOCaml.execute con ?name ~params () in
        let result =
          try
            let row = PGOCaml.next rs in
            if Array.length row = 0 then None else row.(0)
          with PGOCaml.Ready_for_query -> None
        in
        finally();
        result
      with ex -> (finally(); raise ex)

  end












