type key = Symbol.t
type t

val empty : t
val add1 : key -> t -> t
val extend : key list -> t -> t
val find : key -> t -> int32
val mem : key -> t -> bool

val keyFormat : key -> string
val toString : (int32 -> string) -> t -> string
