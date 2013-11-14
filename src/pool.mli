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


(** Generic object pooling. *)

module type POOLED_OBJECT =
  sig
    type p
    type t
    val create : p -> t
    val is_valid : t -> bool
    val destroy : t -> unit
  end

module type S =
  sig
    type pobj
    type param
    type pool
    val verbose : int ref
    val create : ?initial_size:int -> ?max_size:int -> ?expiration_time:float -> param -> pool
    val checkin : pool -> pobj -> unit
    val checkout : pool -> pobj
    val length : pool -> int * int
    val clean_up : pool -> unit
    val destroy : pool -> unit
  end

module Make (PObj : POOLED_OBJECT) : S with type pobj = PObj.t and type param = PObj.p