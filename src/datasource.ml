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

module type DRIVER =
  sig
    type connection
    val connect : host:string -> port:int -> user:string -> password:string -> database:string -> unit -> connection
    val disconnect : connection -> unit
  end


module type S =
  sig
    type t
    type connection
    val verbose : int ref
    val create :
      host:string ->
      port:int ->
      user:string -> password:string -> database:string ->
      ?pool_initial_size:int ->
      ?pool_max_size:int ->
      ?pool_expiration_time:float ->
      unit -> t
    val get_connection : t -> connection
    val release_connection : t -> connection -> unit
    val clear_pool : t -> unit
    val pool_size : t -> int * int
  end

module Make (Driver : DRIVER) =
struct

  module Connection =
    struct
      type p = {
        host : string;
        port : int;
        user : string;
        password : string;
        database : string;
      }
      type t = Driver.connection
      let create param =
        Driver.connect ~host:param.host ~port:param.port
          ~user:param.user ~password:param.password ~database:param.database ()
      let is_valid con = true
      (** TODO: Implementare le strategie di validazione. *)
      let destroy = Driver.disconnect
    end

  module ConnectionPool = Pool.Make(Connection)

  type connection = Connection.t

  type t = {
    param : Connection.p;
    pool : ConnectionPool.pool
  }

  type pool_config = {
    initial_size : int option;
    max_size : int option;
    expiration_time: float option;
  }

  let verbose = ConnectionPool.verbose

  let create ~host ~port ~user ~password ~database
    ?pool_initial_size
    ?pool_max_size
    ?pool_expiration_time () =
    let param = {
      Connection.host = host;
      port = port;
      user = user;
      password = password;
      database = database;
    } in {
      param = param;
      pool = ConnectionPool.create param
        ?initial_size:pool_initial_size
        ?max_size:pool_max_size
        ?expiration_time:pool_expiration_time
    }

  let get_connection ds = ConnectionPool.checkout ds.pool

  let release_connection ds = ConnectionPool.checkin ds.pool

  let clear_pool ds = ConnectionPool.destroy ds.pool

  let pool_size ds = ConnectionPool.length ds.pool

end



