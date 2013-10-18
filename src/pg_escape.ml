(*

  "datasource"
  Copyright (C) 2010-2013 Francesco Tovagliari

  This file is part of "datasource".

  "datasource" is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  "datasource" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


open DatasourceUtil

(** string_of_bytea_from_string *)
let string_of_bytea_from_string =
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

(** iter_bytea_from_channel *)
let iter_bytea_from_channel ?(bufsize=4096) inchan f =
  let buf = String.create bufsize in
  begin
    try
      while true do
        let len = input inchan buf 0 bufsize in
        if len = 0 then raise Exit else (f (string_of_bytea_from_string (String.sub buf 0 len)))
      done;
    with Exit -> ()
  end;;

(*(** escape_chan *)
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
  List.rev !result*)

(** string_of_bytea_from_channel *)
let string_of_bytea_from_channel ?bufsize inchan =
  let chunks = ref [] in
  iter_bytea_from_channel ?bufsize inchan (fun chunk -> chunks := chunk :: !chunks);
  List.rev !chunks;;

(** Alias for [string_of_bytea_from_string]. *)
let escape = string_of_bytea_from_string;;

(** bytea_of_string *)
type unescape_result = Complete of string | Partial of string

let bytea_of_string =
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
    begin
      try
        while !i < String.length str do
          let ch = String.unsafe_get str !i in
          if ch = bs then begin
            let next1 = incr i; String.unsafe_get str !i in
            if next1 = apo then Buffer.add_char buf apo
            else begin (* next1 deve essere '\\' *)
              let next2 = incr i; String.unsafe_get str !i in
              let next3 = incr i; String.unsafe_get str !i in
              if next2 = bs && next3 = bs then Buffer.add_char buf bs
              else begin
                let next4 = incr i; String.unsafe_get str !i in
                Buffer.add_char buf (char (next2, next3, next4))
              end
            end
          end else (Buffer.add_char buf ch);
          incr i;
        done;
        Complete (Buffer.contents buf)
      with Failure "int_of_string" -> Partial (Buffer.contents buf)
    end;;

(** Alias for [Postgres.bytea_of_string]. *)
let unescape = bytea_of_string

(** unescape_unsafe *)
let unescape_unsafe data =
  match bytea_of_string data with
    | Partial data -> data
    | Complete data -> data;;
