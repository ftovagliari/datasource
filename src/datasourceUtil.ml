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


let ( /* ) x f = f x and ( */ ) f x = f x

(** finally *)
let finally f1 f2 =
  try
    let result = f1 () in
    f2 ();
    result
  with ex -> begin
    f2 ();
    raise ex
  end

(** list_filter_map *)
let rec list_filter_map f = function
  | [] -> []
  | a :: b ->
    begin
      match f a with
        | Some x -> x :: (list_filter_map f b)
        | None -> (list_filter_map f b)
    end

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

