module type DRIVER =
  sig
    type connection
    val connect :
      host:string ->
      port:int ->
      user:string -> password:string -> database:string ->
      unit -> connection
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

module Make (Driver : DRIVER) : S with type connection = Driver.connection

