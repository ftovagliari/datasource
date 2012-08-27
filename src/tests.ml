#load "unix.cma";;

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
;;

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
;;

(** string_of_bytea *)
let string_of_bytea b =
  let len = String.length b in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let c = b.[i] in
    let cc = Char.code c in
    if cc < 0x20 || cc > 0x7e then
      Buffer.add_string buf (Printf.sprintf "\\%03o" cc) (* non-print -> \ooo *)
    else if c = '\\' then
      Buffer.add_string buf "\\\\" (* \ -> \\ *)
    else
      Buffer.add_char buf c
  done;
  Buffer.contents buf;;

let escape = string_of_bytea

(** string_of_bytea *)
let string_of_bytea =
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
    Buffer.contents result;;

(** escape_chan_func *)
let escape_chan_func ?(bufsize=4096) inchan f =
  let buf = String.create bufsize in
  begin
    try
      while true do
        let len = input inchan buf 0 bufsize in
        if len = 0 then raise Exit else (f (string_of_bytea (String.sub buf 0 len)))
      done;
    with Exit -> ()
  end;;

(** escape_chan *)
let escape_chan ?bufsize inchan =
  let chunks = ref [] in
  escape_chan_func ?bufsize inchan (fun chunk -> chunks := chunk :: !chunks);
  List.rev !chunks;;

(** escape_chan_old *)
let escape_chan_old ?(bufsize=4096) inchan =
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
            Buffer.reset rbuf;
            add buf length
          end
        end
      done;
    with End_of_file -> (result := Buffer.contents rbuf :: !result)
  end;
  List.rev !result;;



(** read_file *)
let read_file filename =
  let ichan = open_in_bin filename in
  let finally () = close_in ichan in
  try
    let length = in_channel_length ichan in
    let data = Buffer.create length in
    Buffer.add_channel data ichan length;
    finally ();
    Printf.printf "----> %d\n%!" (Buffer.length data);
    Buffer.contents data;
  with ex -> (finally(); raise ex);;

(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.printf "%s: %f ms\n%!" label ((Unix.gettimeofday() -. time) *. 1000.);
  in
  let time = Unix.gettimeofday() in
  let result = try f x with e -> begin
    finally time;
    raise e
  end in
  finally time;
  result;;


let filename = "C:\\ocaml\\devel\\datasource\\test.jpg";;
let test = read_file filename;;
let sum ll = List.length ll, List.fold_left (fun sum chunck -> sum + String.length chunck) 0 ll;;

crono ~label:"escape_slow" (fun () -> ignore(escape_slow test)) ();;

crono ~label:"escape" (fun () -> ignore(escape test)) ();;
crono ~label:"escape_chan" (fun () -> let chan = open_in_bin filename in let data = escape_chan chan in close_in chan; sum data) ();;
crono ~label:"escape_chan_func" (fun () ->
  let sum = ref 0 in let chan = open_in_bin filename in let _ = escape_chan_func chan (fun x -> sum := !sum + String.length x) in close_in chan; !sum) ();;
crono ~label:"escape_chan_old" (fun () -> let chan = open_in_bin filename in let data = escape_chan_old ~bufsize:10_000 chan in close_in chan; sum data) ();;
crono ~label:"string_of_bytea" (fun () -> ignore(string_of_bytea test)) ();;


let _ =
  let filename = "C:\\ocaml\\devel\\datasource\\test.jpg" in
  Printf.printf "Size of %s is %d\n%!" filename (Unix.stat filename).Unix.st_size;
  let chan = open_in_bin filename in
  let bufsize = 4096 in
  let buf = String.create bufsize in
  try
    let sum = ref 0 in
    while
      let len = input chan buf 0 bufsize in
      (*Printf.printf "%d\n%!" len;*)
      sum := !sum + len;
      len > 0
    do ()
    done;
    close_in_noerr chan;
    Printf.printf "%d, %!" !sum
  with ex -> (close_in_noerr chan);;

let _ =
  let filename = "C:\\ocaml\\devel\\datasource\\test.jpg" in
  Printf.printf "Size of %s is %d\n%!" filename (Unix.stat filename).Unix.st_size;
  let chan = open_in_bin filename in
  let bufsize = 10_000 in
  let buf = String.create bufsize in
  let chuncks = ref [] in
  begin
    try
      while true do
        let len = input chan buf 0 bufsize in
        if len = 0 then raise Exit else begin
          chuncks := (String.sub buf 0 len) :: !chuncks
        end
      done;
      close_in_noerr chan;
    with ex -> (close_in_noerr chan);
  end;
  Printf.printf "---> %d\n%!" (List.length !chuncks);
  print_int (List.fold_left (fun sum chunck -> sum + String.length chunck) 0 !chuncks);;





