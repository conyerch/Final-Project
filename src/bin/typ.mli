(* file: typ.mli
   author: Robert Muller
   date: Feb 20, 2009
*)
type t = Int
       | Bool
       | Void
       | Arrow of {from : t; too : t}
       | Product of t list [@@deriving sexp]

val format : t -> string

val equal : t -> t -> bool
