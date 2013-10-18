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

open PGOCaml

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
  result


module PGDriver =
  struct
    type connection = unit PGOCaml.t
    let connect ~host ~port ~user ~password ~database () =
      PGOCaml.connect ~host ~port ~user ~password ~database ()
    let disconnect = PGOCaml.close
  end
module PGDatasource = Datasource.Make(PGDriver)

let meteo = PGDatasource.create
  ~pool_initial_size:3
(*  ~pool_max_size:4*)
  ~pool_expiration_time:100.
  ~host:"127.0.0.1" ~port:5432 ~user:"meteo" ~password:"meteo" ~database:"meteo" ()



(*module FastPGDriver =
  struct
    type connection = unit FastPGOCaml.t
    let connect ~host ~port ~user ~password ~database () =
      FastPGOCaml.connect ~host ~port ~user ~password ~database ()
    let disconnect = FastPGOCaml.close
  end
module FastPGDatasource = Datasource.Make(FastPGDriver)*)

module FastPGDatasource = Postgres.Datasource

let fast_meteo = FastPGDatasource.create
  ~pool_initial_size:0
(*  ~pool_max_size:4*)
  ~pool_expiration_time:100.
  ~host:"127.0.0.1" ~port:5432 ~user:"meteo" ~password:"meteo" ~database:"meteo" ()



let connect () = crono ~label:"connect (pool)" PGDatasource.get_connection meteo
let disconnect = PGDatasource.release_connection meteo

let ll_connect () = crono ~label:"connect (ll)" (PGOCaml.connect
  ~host:"127.0.0.1" ~port:5432 ~user:"meteo" ~password:"meteo" ~database:"meteo") ()
let ll_disconnect = PGOCaml.close


let worker n =
  Printf.printf "Thread %d partito.\n%!" n;
  Thread.delay (Random.float 10.0);
  let con = connect() in
  Thread.delay (Random.float 5.0);
  disconnect con

let main () = begin
  let n_threads = 100 in
  let threads = ref [] in
  for i = 1 to n_threads do
(*    let con = ll_connect () in ll_disconnect con;*)
(*    let con = connect() in disconnect con*)
    threads := (Thread.create worker i) :: !threads;
  done;
  List.iter Thread.join !threads;
  Printf.printf "FINE\n%!"
end


let list_min l = List.hd (List.sort compare l)
let list_max l = List.hd (List.sort (fun x y -> (-1) * (compare x y)) l)
let remove_bounds l = List.tl (List.rev (List.tl (List.sort compare l)))
let string_of_result = function None -> "(null)" | Some s -> s

let speed_test n =
  let query = "select * from meteo.dati /*limit 500*/" in
  let name = "speed_test....." in
  let test () =
    let con = PGDatasource.get_connection meteo in
    PGOCaml.prepare con ~name ~query ();
    let times = Array.create n 0. in
    for i = 0 to n - 1 do
      let time = Unix.gettimeofday () in
      let results = PGOCaml.execute con ~name ~params:[] () in
      List.iter begin function
        | [id; data; min; max; pg; tipo_pg; cielo; note] as row ->
          let row = String.concat "\t" (List.map string_of_result row) in
(*          Printf.printf "%s\n%!" row*) ()
        | _ -> assert false
      end results;
      times.(i) <- (Unix.gettimeofday ()) -. time;
    done;
    PGOCaml.close_statement con ~name ();
    PGDatasource.release_connection meteo con;
    times
  in
  let t1 = test () in
  let name = "speed_test_fast" in
  let test () =
    let con = FastPGDatasource.get_connection fast_meteo in
    FastPGOCaml.prepare con ~name ~query ();
    let times = Array.create n 0. in
    for i = 0 to n - 1 do
      let time = Unix.gettimeofday () in
      let results = FastPGOCaml.execute con ~name ~params:[] () in
      begin
        try while true do
          match FastPGOCaml.next results with
            | [|id; data; min; max; pg; tipo_pg; cielo; note|] as row ->
              let buf = Buffer.create 1000 in
              Array.iter (fun x -> Buffer.add_string buf (string_of_result x); Buffer.add_char buf '\t') row;
              let row = Buffer.contents buf in
(*              Printf.printf "%s\n%!" row*)()
(*              Printf.printf "%s %s %s %s %s %s %s %s \n%!"
              id
              (string_of_result data)
              (string_of_result min)
              (string_of_result max)
              (string_of_result pg)
              (string_of_result tipo_pg)
              (string_of_result cielo)
              (string_of_result note)*)
            | _ -> assert false
        done
        with FastPGOCaml.Ready_for_query -> ()
      end;
      times.(i) <- (Unix.gettimeofday ()) -. time;
    done;
    FastPGOCaml.close_statement con ~name ();
    FastPGDatasource.release_connection fast_meteo con;
    times
  in
  let t2 = test () in
  let sum, mi, ma = ref min_float, ref max_float, ref 0. in
  let t1, t2 = remove_bounds (Array.to_list t1), remove_bounds (Array.to_list t2) in
  List.iter2 begin fun t1 t2 ->
    let ratio = 1. -. t2 /. t1 in
    sum := !sum +. ratio;
    mi := min !mi ratio;
    ma := max !ma ratio;
  end t1 t2;
  Printf.printf "------------------> avg = %.2f; min = %.2f; max = %.2f\n%!"
    (!sum /. (float n) *. 100.) (!mi *. 100.) (!ma *. 100.)


module SimpleQuery = Postgres.SimpleQuery

let _ = begin
  Random.self_init();
(*  Printexc.print main ()*)
  Printexc.print speed_test 10;
  Printf.printf "SimpleQuery test:\n%!";
  let buf = Buffer.create 1000 in
  SimpleQuery.query
    ~name:"simple_query_test"
    ~datasource:fast_meteo
    ~sql:"SELECT * FROM meteo.dati LIMIT $1"
    ~params:[Some "10"]
    begin fun row ->
      Array.iter begin function
        | None -> Buffer.add_string buf "(null)\t"
        | Some x -> Buffer.add_string buf x; Buffer.add_char buf '\t';
      end row;
      Buffer.add_char buf '\n';
    end;
    Buffer.output_buffer stdout buf
end





