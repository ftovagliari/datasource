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

module Datasource = Datasource.Make (
  struct
    type connection = unit FastPGOCaml.t
    let connect ~host ~port ~user ~password ~database () =
      FastPGOCaml.connect ~host ~port ~user ~password ~database ()
    let disconnect = FastPGOCaml.close
  end);;

module Escape = struct include Pg_escape end

module Make (X : sig val datasource : Datasource.t end) = struct

  open DatasourceUtil
  open Printf
  open CalendarLib

  let get_connection () =
    let rec try_connect attempts =
      try Datasource.get_connection X.datasource
      with Unix.Unix_error (err, a, b) as ex ->
        if attempts = 0 then (raise ex) else (try_connect (attempts - 1))
    in try_connect 10

  let release_connection = Datasource.release_connection X.datasource
  let clear_pool () = Datasource.clear_pool X.datasource
  let pool_size () = Datasource.pool_size X.datasource

  let escape_sql_char =
    let re = Str.regexp "'" in
    Str.global_replace re "''"

  let create_sql_char str = "'" ^ (escape_sql_char str) ^ "'"

  (** column functions *)
  let column_text_default default row i =
    match row.(i) with None -> default | Some txt -> txt

  let column_text row i =
    match row.(i) with None -> assert false | Some txt -> txt

  let column_float_opt row i =
    match row.(i) with None -> None | Some n -> Some (float_of_string n)

  let column_float_default default row i =
    match row.(i) with None -> default | Some n -> float_of_string n

  let column_float row i =
    match row.(i) with None -> assert false | Some flo -> float_of_string flo

  let column_float_timestamp_opt row i =
    match row.(i) with None -> None | Some ts -> Some (Calendar.to_unixfloat (FastPGOCaml.timestamp_of_string ts))

  let column_float_timestamp_default default row i =
    match row.(i) with None -> default | Some ts -> Calendar.to_unixfloat (FastPGOCaml.timestamp_of_string ts)

  let column_int row i =
    match row.(i) with None -> assert false | Some n -> int_of_string n

  let column_int_opt row i =
    match row.(i) with None -> None | Some n -> Some (int_of_string n)

  let column_int_default default row i =
    match row.(i) with None -> default | Some i -> int_of_string i

  let column_bool row i =
    match row.(i) with None -> assert false | Some "t" -> true | _ -> false

  let column_bool_opt row i =
    match row.(i) with None -> None | Some b -> Some (FastPGOCaml.bool_of_string b)

  (** bind functions *)
  let bind_float_opt flopt =
    match flopt with None -> None | Some flo -> Some (string_of_float flo)
  let bind_int_opt n =
    match n with None -> None | Some n -> Some (string_of_int n)

  let bind_text txt = Some txt
  let bind_text_opt txt = txt
  let bind_int n = Some (string_of_int n)
  let bind_float n = Some (string_of_float n)
  let bind_bool b = Some (string_of_bool b)

  let bind_timestamp x = FastPGOCaml.string_of_timestamp (Calendar.from_unixfloat x)
  let bind_timestamp_opt = function None -> None | Some x -> Some (bind_timestamp x)

  (** select_first *)
  let select_first =
    let count = ref 0 in
    fun ?db ?name sql ->
      let name = match name with None -> incr count; Printf.sprintf "select_first-%d" !count | Some x -> x in
      let con = match db with None -> get_connection () | Some con -> con in
      let is_prepared = ref false in
      begin fun () ->
        let stmt = FastPGOCaml.prepare con ~name ~query:sql () in
        is_prepared := true;
        let rs = FastPGOCaml.execute con ~name ~params:[] () in
        begin fun () ->
          try
            let row = FastPGOCaml.next rs in
            if Array.length row = 0 then None else row.(0);
          with FastPGOCaml.Ready_for_query -> None
        end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs)
      end /*finally*/ begin fun () ->
        if !is_prepared then (FastPGOCaml.close_statement ~name con ());
        match db with None -> release_connection con | _ -> ();
      end;;

  (** select_first_2 *)
  let select_first_2 ?db ?name sql =
    let name = match name with None -> Printf.sprintf "select_first_pair-%f" (Unix.gettimeofday()) | Some x -> x in
    let con = match db with None -> get_connection () | Some con -> con in
    let is_prepared = ref false in
    begin fun () ->
      let stmt = FastPGOCaml.prepare con ~name ~query:sql () in
      is_prepared := true;
      let rs = FastPGOCaml.execute con ~name ~params:[] () in
      begin fun () -> try
          let row = FastPGOCaml.next rs in
          let len = Array.length row in
          if len = 0 then None else Some (row.(0), if len = 1 then None else row.(1));
        with FastPGOCaml.Ready_for_query -> None
      end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs)
    end /*finally*/ begin fun () ->
      if !is_prepared then (FastPGOCaml.close_statement ~name con ());
      match db with None -> release_connection con | _ -> ();
    end

  (** select_first_3 *)
  let select_first_3 ?db ?name sql =
    let name = match name with None -> Printf.sprintf "select_first_3-%f" (Unix.gettimeofday()) | Some x -> x in
    let con = match db with None -> get_connection () | Some con -> con in
    let is_prepared = ref false in
    begin fun () ->
      let stmt = FastPGOCaml.prepare con ~name ~query:sql () in
      is_prepared := true;
      let rs = FastPGOCaml.execute con ~name ~params:[] () in
      begin fun () -> try
          let row = FastPGOCaml.next rs in
          begin match Array.length row with
            | n when n >= 3 -> Some (row.(0), row.(1), row.(2))
            | 1 -> Some (row.(0), None, None)
            | 2 -> Some (row.(0), row.(1), None)
            | _ -> None
          end
        with FastPGOCaml.Ready_for_query -> None
      end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs)
    end /*finally*/ begin fun () ->
      if !is_prepared then (FastPGOCaml.close_statement ~name con ());
      match db with None -> release_connection con | _ -> ();
    end

  (** select_first_4 *)
  let select_first_4 ?db ?name sql =
    let name = match name with None -> Printf.sprintf "select_first_4-%f" (Unix.gettimeofday()) | Some x -> x in
    let con = match db with None -> get_connection () | Some con -> con in
    let is_prepared = ref false in
    begin fun () ->
      let stmt = FastPGOCaml.prepare con ~name ~query:sql () in
      is_prepared := true;
      let rs = FastPGOCaml.execute con ~name ~params:[] () in
      begin fun () -> try
          let row = FastPGOCaml.next rs in
          begin match Array.length row with
            | n when n >= 4 -> Some (row.(0), row.(1), row.(2), row.(3))
            | 1 -> Some (row.(0), None, None, None)
            | 2 -> Some (row.(0), row.(1), None, None)
            | 3 -> Some (row.(0), row.(1), row.(2), None)
            | _ -> None
          end
        with FastPGOCaml.Ready_for_query -> None
      end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs)
    end /*finally*/ begin fun () ->
      if !is_prepared then (FastPGOCaml.close_statement ~name con ());
      match db with None -> release_connection con | _ -> ();
    end

(** select_first_array *)
let select_first_array ?db ?name ?(params=[]) sql =
  let name = match name with None -> Printf.sprintf "select_first_array%f" (Unix.gettimeofday()) | Some x -> x in
  let con = match db with None -> get_connection () | Some con -> con in
  let is_prepared = ref false in
  begin fun () ->
    let stmt = FastPGOCaml.prepare con ~name ~query:sql () in
    is_prepared := true;
    let rs = FastPGOCaml.execute con ~name ~params () in
    begin fun () -> try
      FastPGOCaml.next rs
    with FastPGOCaml.Ready_for_query -> [||]
    end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs)
  end /*finally*/ begin fun () ->
    if !is_prepared then (FastPGOCaml.close_statement ~name con ());
    (match db with None -> release_connection con | _ -> ());
  end

  (** Itera la funzione [finish] su tutti i risultati restituita dalla select.
   * Se [finish] restituisce [true], il ciclo di iterazioni si interrompe e
   * [select_iter] termina; altrimenti continua sul prossimo risutato.
   *
   * NON NIDIFICARE UNA CHIAMATA DENTRO AD UN'ALTRA.
   *
  *)
  let select_iter ?db ?name ?meta ?(params=[]) finish sql =
    let name = match name with None -> Printf.sprintf "select_iter-%f" (Unix.gettimeofday()) | Some x -> x in
    let con = match db with None -> get_connection () | Some con -> con in
    let is_prepared = ref false in
    begin fun () ->
      let _ = FastPGOCaml.prepare con ~name ~query:sql () in
      is_prepared := true;
      (match meta with None -> () | Some m -> m := Some (FastPGOCaml.describe_statement con ~name ()));
      let rs = FastPGOCaml.execute con ~name ~params () in
      begin fun () ->
        try
          while not (finish (FastPGOCaml.next rs)) do () done;
        with FastPGOCaml.Ready_for_query -> ()
      end /*finally*/ (fun () -> FastPGOCaml.close_result_set rs);
    end /*finally*/ begin fun () ->
      if !is_prepared then (FastPGOCaml.close_statement ~name con ());
      match db with None -> release_connection con | _ -> ();
    end

  (** prepare_and_exec
    * @param params è una lista di liste perché lo stesso SQL viene eseguito
    * una volta per ogni ennupla di parametri.
    * @param returning
  *)
  let prepare_and_exec db ?name ?(params=[[]])
      ?(returning=(fun rs -> ignore (FastPGOCaml.next rs)))
      sql =
    let name = match name with Some x -> x | None -> Printf.sprintf "prepare_and_exec_%f" (Unix.gettimeofday()) in
    let is_prepared = ref false in
    begin fun () ->
      let stmt = FastPGOCaml.prepare db ~name ~query:sql () in
      is_prepared := true;
      List.iter begin fun params ->
        let rs = FastPGOCaml.execute db ~name ~params () in
        try
          ignore (returning rs);
          FastPGOCaml.close_result_set rs
        with FastPGOCaml.Ready_for_query -> ()
      end params;
    end /*finally*/ begin fun () ->
      if !is_prepared then (FastPGOCaml.close_statement ~name db ());
    end

  (** new_savepoint_name *)
  let new_savepoint_name =
    let count = ref 0 in fun () -> incr count; Printf.sprintf "sp%d" !count

  (** execute_transaction *)
  let execute_transaction f =
    let db = get_connection () in
    begin fun () ->
      prepare_and_exec db "BEGIN TRANSACTION;";
      try
        let result = f db in
        prepare_and_exec db "COMMIT;";
        result
      with ex -> begin
          prepare_and_exec db "ROLLBACK;";
          raise ex
        end
    end /*finally*/ (fun () -> release_connection db)

  (** Conta i risultati che restituisce una query eseguendo un [SELECT count( * ) FROM ...] *)
  let count_results =
    let re1 = Str.regexp "[\n\r\t]" in
    let re2 = Str.regexp "\\(SELECT\\).+\\(FROM.+\\)" in
    let re3 = Str.regexp "ORDER BY .*" in
    fun ~db sql ->
      let sql_count = Str.global_replace re1 "" sql in
      let sql_count = Str.replace_first re2 "\\1 count( * ) \\2" sql_count in
      let sql_count = Str.replace_first re3 "" sql_count in
      let count = match select_first ~db sql_count with None -> 0 | Some x -> int_of_string x in
      count

  (** with_connection *)
  let with_connection f =
    let db = get_connection () in
    try
      let result = f db in
      release_connection db;
      result
    with ex -> begin
        release_connection db;
        raise ex
      end

  type value = NULL | TIMESTAMP of float | TEXT of string

  (** create_update_set *)
  let create_update_set fields =
    String.concat "," (list_filter_map begin fun (n, v) ->
        match v with
          | None -> None
          | Some NULL -> Some (n^"=NULL")
          | Some (TIMESTAMP x) -> Some (n^"="^(create_sql_char (bind_timestamp x)))
          | Some (TEXT x) -> Some (n^"="^(create_sql_char x))
      end fields);;


  (** get_column_names *)
  let get_column_names ~db ~tabname =
    let sql = sprintf "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'" tabname in
    let result = ref [] in
    select_iter ~db begin function
      | [| Some name |] ->
        result := name :: !result;
        false
      | _ -> assert false
    end sql;
    List.rev !result;;


end



