module Datasource = Datasource.Make (
  struct
    type connection = Sqlite3.t
    let connect ~host ~port ~user ~password ~database () =
      Sqlite3.open_file database
    let disconnect = Sqlite3.close
  end
)
