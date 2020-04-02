type cassSession

type cassStatement

type cassCluster

type cassFuture

type cassError

type cassResult

type cassRow

type cassValue

type cassIterator

external ml_cass_session_new : unit -> cassSession = "cass_session_new"

external ml_cass_cluster_new : unit -> cassCluster = "cass_cluster_new"

external ml_cass_cluster_set_contact_points : cassCluster -> string -> unit
  = "cass_cluster_set_contact_points"

external ml_cass_session_connect : cassSession -> cassCluster -> cassFuture
  = "cass_session_connect"

external ml_cass_future_wait : cassFuture -> unit = "cass_future_wait"

external ml_cass_future_error_code : cassFuture -> cassError
  = "cass_future_error_code"

external ml_cass_cluster_free : cassCluster -> unit = "cass_cluster_free"

external ml_cass_session_free : cassSession -> unit = "cass_session_free"

external ml_cass_statement_new : string -> int -> cassStatement
  = "cass_statement_new"

external ml_cass_session_execute : cassSession -> cassStatement -> cassFuture
  = "cass_session_execute"

external ml_cass_future_free : cassFuture -> unit = "cass_future_free"

external ml_cass_statement_free : cassStatement -> unit = "cass_statement_free"

external ml_cass_statement_bind_string : cassStatement -> int -> string -> unit
  = "cass_statement_bind_string"

external ml_cass_future_get_result : cassFuture -> cassResult
  = "cass_future_get_result"

external ml_cass_result_first_row : cassResult -> cassRow
  = "cass_result_first_row"

external ml_cass_row_get_column : cassRow -> int -> cassValue
  = "cass_row_get_column"

external ml_cass_iterator_from_result : cassResult -> cassIterator
  = "cass_iterator_from_result"

external ml_cass_result_row_count : cassResult -> int = "cass_result_row_count"

external ml_cass_iterator_next : cassIterator -> bool = "cass_iterator_next"

external ml_cass_iterator_get_row : cassIterator -> cassRow
  = "cass_iterator_get_row"

external ml_cass_row_get_column_by_name : cassRow -> string -> cassValue
  = "cass_row_get_column_by_name"

external ml_cass_session_close : cassSession -> cassFuture
  = "cass_session_close"

external cstub_get_string : cassValue -> string = "get_string"

external cstub_convert : int -> int = "convert"

external cstub_match_enum : cassError -> cassFuture -> bool = "match_enum"

external cstub_convert_to_bool : bool -> bool = "convert_to_bool"

external cstub_convert_to_ml : int -> int = "convert_to_ml"


type t = cassSession
type error = string

val create_keyspace : string
val create_dir_table : string
val create_file_table : string

val execute_query : t -> string -> (cassFuture, error) Lwt_result.t
val execute_query' : t -> string -> cassFuture

module File : Git.FILE 
  with type t = t 
   and type error = string
   and type 'a fd = (t * string * string) ref

module Mapper : Git.MAPPER
  with type t = t
   and type error = string
   and type fd = (t * string * string) ref

module Dir : Git.DIR 
  with type t = t 
   and type error = string
            
module Fs : Git.FS
  with type t = t
   and type error = string
   and module File = File
   and module Mapper = Mapper
   and module Dir = Dir
