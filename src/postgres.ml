(*

  "datasource"
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

module PGOCaml = FastPGOCaml

module Datasource = Datasource.Make (struct
  type connection = unit PGOCaml.t
  let connect ~host ~port ~user ~password ~database () =
    PGOCaml.connect ~host ~port ~user ~password ~database ()
  let disconnect = PGOCaml.close;
end);;

module Util = struct
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
    String.concat "" !result;;

  (** read_file *)
  let read_file filename =
    let ichan = open_in_bin filename in
    let finally () = close_in ichan in
    try
      let length = in_channel_length ichan in
      let data = Buffer.create length in
      Buffer.add_channel data ichan length;
      finally ();
      Buffer.contents data;
    with ex -> (finally(); raise ex);;
end

(** Iterativa *)

(** memo *)
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
      data;;

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

(** Alias for [Postgres.string_of_bytea]. *)
let escape = string_of_bytea;;

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

(** string_of_bytea_chan_func *)
let string_of_bytea_chan_func ?(bufsize=4096) inchan f =
  let buf = String.create bufsize in
  begin
    try
      while true do
        let len = input inchan buf 0 bufsize in
        if len = 0 then raise Exit else (f (string_of_bytea (String.sub buf 0 len)))
      done;
    with Exit -> ()
  end;;

(** string_of_bytea_chan *)
let string_of_bytea_chan ?bufsize inchan =
  let chunks = ref [] in
  string_of_bytea_chan_func ?bufsize inchan (fun chunk -> chunks := chunk :: !chunks);
  List.rev !chunks;;

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

(** SimpleQuery *)
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












