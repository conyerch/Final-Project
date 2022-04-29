open Sexplib.Std

(* file: symbol.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This module contains the representation of symbols.
*)
type t = string [@@deriving sexp]

let fromString s = s
let format sym = sym
let fresh() =
  Lib.fmt "x%d" (Lib.fresh ())

let compare = String.compare

type binding = { id  : t
               ; typ : Typ.t
               } [@@deriving sexp]

let b2s {id; typ} =
  Lib.fmt "%s : %s" (format id) (Typ.format typ)

(* make a string for a binding occurrence: x:t
*)
let toStringBindOcc {id; typ} =
  let ids = format id in
  let typs = Typ.format typ
  in
  Lib.fmt "%s : %s" ids typs
