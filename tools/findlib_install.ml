open Printf

(** pushd and popd *)
let pushd, popd =
  let stack = Stack.create () in
  begin fun dir ->
    let cwd = Sys.getcwd () in
    Stack.push cwd stack;
    Sys.chdir dir
  end, (fun () -> Sys.chdir (Stack.pop stack))

let libname = "datasource"
let cmas = ["datasource"; "pgdatasource"]

let _ =
  let ar = String.concat " " (List.map (fun x -> x ^ (if Sys.os_type = "Win32" then ".lib" else ".a")) cmas) in
  let find ext =
    let files = Array.to_list (Sys.readdir ".") in
    let files = List.filter (fun x -> Filename.check_suffix x ext) files in
    String.concat " " files
  in
  let cmas = List.map (fun x -> [x ^ ".cma"; x ^ ".cmxa"]) cmas in
  let cmas = String.concat " " (List.flatten cmas) in
  ignore (kprintf Sys.command "ocamlfind install %s META %s %s %s %s" libname cmas ar (find ".cmi") (find ".mli"));
;;
