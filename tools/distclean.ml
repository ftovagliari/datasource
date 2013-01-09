open Printf

let remove file = if Sys.file_exists file then Sys.remove file

let (//) = Filename.concat

let filter ext = List.filter (fun x -> Filename.check_suffix x ext)

let readdir dir = Array.to_list (Array.map (fun x -> dir // x) (Sys.readdir dir))

let rmr dir =
  let rmr = if Sys.os_type = "Win32" then "RMDIR /Q /S" else "rm -fr" in
  ignore (kprintf Sys.command "%s %s" rmr dir)

let _ =
  List.iter remove (filter ".html" (readdir (".." // "doc")));
  List.iter remove (filter ".css" (readdir (".." // "doc")));
  rmr (".." // ".tmp");
  rmr (".." // ".cache");
  rmr (".." // "bak");
;;
