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
    type connection = Sqlite3.t
    let connect ~host ~port ~user ~password ~database () =
      Sqlite3.open_file database
    let disconnect = Sqlite3.close
  end
)
