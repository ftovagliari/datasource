(** {6 Datasource for PostgreSQL connections} *)
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

(** {4 Escaping and unescaping [bytea] strings } *)
module Escape :
  sig
    val string_of_bytea_from_string : string -> string
    val iter_bytea_from_channel :
      ?bufsize:int -> in_channel -> (string -> 'a) -> unit
    val string_of_bytea_from_channel :
      ?bufsize:int -> in_channel -> string list
    val escape : string -> string
    type unescape_result =
      Pg_escape.unescape_result =
        Complete of string
      | Partial of string
    val bytea_of_string : string -> unescape_result
    val unescape : string -> unescape_result
    val unescape_unsafe : string -> string
  end

(**  *)
module Make :
  functor (X : sig val datasource : Datasource.t end) ->
    sig
      val get_connection : unit -> Datasource.connection
      val release_connection : Datasource.connection -> unit
      val clear_pool : unit -> unit
      val pool_size : unit -> int * int
      val escape_sql_char : string -> string
      val create_sql_char : string -> string
      val column_text_default : 'a -> 'a option array -> int -> 'a
      val column_text : 'a option array -> int -> 'a
      val column_float_opt : string option array -> int -> float option
      val column_float_default : float -> string option array -> int -> float
      val column_float : string option array -> int -> float
      val column_float_timestamp_opt :
        string option array -> int -> float option
      val column_float_timestamp_default :
        float -> string option array -> int -> float
      val column_int : string option array -> int -> int
      val column_int_opt : string option array -> int -> int option
      val column_int_default : int -> string option array -> int -> int
      val column_bool : string option array -> int -> bool
      val column_bool_opt : string option array -> int -> bool option
      val bind_float_opt : float option -> string option
      val bind_int_opt : int option -> string option
      val bind_text : 'a -> 'a option
      val bind_text_opt : 'a -> 'a
      val bind_int : int -> string option
      val bind_float : float -> string option
      val bind_bool : bool -> string option
      val bind_timestamp : float -> string
      val bind_timestamp_opt : float option -> string option
      val select_first :
        ?db:Datasource.connection -> ?name:string -> string -> string option
      val select_first_2 :
        ?db:Datasource.connection ->
        ?name:string -> string -> (string option * string option) option
      val select_first_3 :
        ?db:Datasource.connection ->
        ?name:string ->
        string -> (string option * string option * string option) option
      val select_first_4 :
        ?db:Datasource.connection ->
        ?name:string ->
        string ->
        (string option * string option * string option * string option)
        option
      val select_first_array :
        ?db:Datasource.connection ->
        ?name:string ->
        ?params:string option list -> string -> string option array
      val select_iter :
        ?db:Datasource.connection ->
        ?name:string ->
        ?meta:(FastPGOCaml.param_description list *
               FastPGOCaml.result_description list option)
              option ref ->
        ?params:string option list ->
        (string option array -> bool) -> string -> unit
      val prepare_and_exec :
        'a FastPGOCaml.t ->
        ?name:string ->
        ?params:string option list list ->
        ?returning:('a FastPGOCaml.result_set -> unit) -> string -> unit
      val new_savepoint_name : unit -> string
      val execute_transaction : (Datasource.connection -> 'a) -> 'a
      val count_results : db:Datasource.connection -> string -> int
      val with_connection : (Datasource.connection -> 'a) -> 'a
      type value = NULL | TIMESTAMP of float | TEXT of string
      val create_update_set : (string * value option) list -> string
      val get_column_names :
        db:Datasource.connection -> tabname:string -> string list
    end
