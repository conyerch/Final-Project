(* file: symbol.mli
   author: Bob Muller

    CSCI 3366 Programming Languages

   This is the interface file for symbols.
*)
type t [@@deriving sexp]
type binding = { id  : t
               ; typ : Typ.t
               } [@@deriving sexp]

val fromString : string -> t
val format : t -> string
val fresh : unit -> t
val compare : t -> t -> int
val b2s : binding -> string
val toStringBindOcc : binding -> string
