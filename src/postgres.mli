module Datasource :
  sig
    type t
    type connection = unit FastPGOCaml.t
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
val escape : string -> string
val escape_chan : ?bufsize:int -> in_channel -> string list
val unescape : string -> string
val read_file : string -> string


module SimpleQuery :
  sig
    val query :
      datasource:Datasource.t ->
      ?name:string ->
      sql:string ->
      ?params:string option list ->
      (string option array -> unit) ->
      unit

    val query_first :
      datasource:Datasource.t ->
      ?name:string ->
      sql:string ->
      ?params:string option list ->
      unit ->
      string option
  end
