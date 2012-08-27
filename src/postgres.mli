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


module Datasource :
  sig
    type connection = unit FastPGOCaml.t
    type t
    val verbose : int ref
    val create :
      host:string ->
      port:int ->
      user:string ->
      password:string ->
      database:string ->
      ?pool_initial_size:int ->
      ?pool_max_size:int -> ?pool_expiration_time:float -> unit -> t
    val get_connection : t -> connection
    val release_connection : t -> connection -> unit
    val clear_pool : t -> unit
    val pool_size : t -> int * int
  end
val read_file : string -> string
val string_of_bytea : string -> string

(** Alias for [Postgres.string_of_bytea].
    @deprecated Use [Postgres.string_of_bytea] *)
val escape : string -> string

val string_of_bytea_chan_func : ?bufsize:int -> in_channel -> (string -> 'a) -> unit
val string_of_bytea_chan : ?bufsize:int -> in_channel -> string list

val bytea_of_string : string -> string

(** Alias for [Postgres.bytea_of_string].
    @Deprecated Use [Postgres.bytea_of_string] *)
val unescape : string -> string
module SimpleQuery :
  sig
    val query :
      datasource:Datasource.t ->
      ?name:string ->
      sql:string ->
      ?params:string option list -> (string option array -> 'a) -> unit
    val query_first :
      datasource:Datasource.t ->
      ?name:string ->
      sql:string -> ?params:string option list -> unit -> string option
  end
