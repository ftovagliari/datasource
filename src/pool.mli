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