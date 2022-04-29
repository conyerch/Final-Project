(* file: env.ml
 * author: Robert Muller
 *
 * CSCI 3366 Programming Languages
 *
 * This file contains the environment code for miniC.
 *)

module M = Map.Make(Symbol)

type key = Symbol.t
type t = { map : int32 M.t;
           next : int32 }


let empty = { map = M.empty; next = Int32.zero }
let add1 k env =
  { map = M.add k env.next env.map; next = Int32.add env.next Int32.one }
let find k env = M.find k env.map
let mem k env = M.mem k env.map

let keyFormat = Symbol.format

let rec extend ks env =
  match ks with
  | [] -> env
  | k :: ks -> extend ks (add1 k env)

let toString stringer env =
  let map = env.map in
  let bindings = M.bindings map in
  let folder s (key, value) =
    Lib.fmt "%s = %s; %s" (Symbol.format key) (stringer value) s
  in
  Lib.fmt "{%s}" (List.fold_left folder "" bindings)
