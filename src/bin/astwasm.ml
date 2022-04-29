(*
 * This is a simplified version of the WASM AST from the WASM reference interpreter.
 * The key thing is we drop all of the annotations about source locations/positions,
 * as we do not need those.
 * 
 * Second, we remove many instructions (e.g. vectors) that are not needed.
 *
 * We try to use the same naming conventions used in the reference interpreter.
 *
 *   x : var
 *   v : value
 *   e : instr
 *   f : func
 *   m : module_
 *
 *   t : value_type
 *   s : func_type
 *   c : context / config
 *
 *)

(*
open Types

type void = Lib.void
   *)

module I32 = Wasm.I32
(* Operators *)

type value_type = I32Type
type stack_type = value_type list
type func_type = FuncType of stack_type * stack_type

type ('i32) op =
  I32 of 'i32

type value = (I32.t) op


(* Injection & projection *)

exception Value of value_type

module type ValueType =
sig
  type t
  val to_value : t -> value
  val of_value : value -> t (* raise Value *)
end

module I32Value =
struct
  type t = I32.t
  let to_value i = I32 i
  let of_value = function I32 i -> i
end


(* Typing *)

(* Comparison *)

(* Defaults *)

let default_value = function
  | I32Type -> I32 I32.zero

(* Conversion *)

let value_of_bool b = I32 (if b then 1l else 0l)

let string_of_value = function
  | I32 i -> I32.to_string_s i

let string_of_values = function
  | [v] -> string_of_value v
  | vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"

module IntOp =
struct
  type unop = Clz | Ctz | Popcnt 
  type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
             | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  type testop = Eqz
  type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
end

module I32Op = IntOp

type testop = (I32Op.testop) op
type unop = (I32Op.unop) op
type binop = (I32Op.binop) op
type relop = (I32Op.relop) op

type ('t, 'p) memop = {ty : 't; align : int; offset : int32; pack : 'p}

(* Expressions *)

type var = int32
type literal = value
type name = int list

and instr =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Drop                              (* forget a value *)
  | Select
  | Block of instr list  (* execute in sequence *)
  | Loop of instr list   (* loop header *)
  | If of instr list * instr list  (* conditional *)
  (** NOTE: In lecture block, loop, and if all took an extra "block_type" argument,
     which we have removed to simplify things further ** *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | LocalGet of var                   (* read local variable *)
  | LocalSet of var                   (* write local variable *)
  | LocalTee of var                   (* write local variable and keep value *)
  | Const of literal                  (* constant *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)

(* Globals & Functions *)

type const = instr list

type func =
{
  ftype : var;
  locals : value_type list;
  body : instr list;
}



(* Modules *)

type type_ = func_type

type export_desc =
  | FuncExport of var
  | TableExport of var
  | MemoryExport of var
  | GlobalExport of var

type export =
{
  name : name;
  edesc : export_desc;
}

type module_ =
{
  types : type_ list;
  funcs : func list;
  start : var option;
  exports : export list;
}


(* Auxiliary functions *)

let empty_module =
{
  types = [];
  funcs = [];
  start = None;
  exports = [];
}

let int32max i1 i2 =
  match compare i1 i2 with
  | 1 -> i1
  | _ -> i2

let rec max_local code =
  match code with
  | [] -> Int32.zero
  | i :: code ->
    let this =
      match i with
      | LocalGet var -> var
      | LocalSet var -> var
      | LocalTee var -> var
      | Block ls -> max_local ls
      | Loop ls -> max_local ls
      | If (ls1, ls2) -> int32max (max_local ls1) (max_local ls2)
      | _ -> Int32.zero
    in
    let rest = max_local code in
    int32max this rest

let locals_types formals code =
  let max_used = max_local code in
  let extra = Int32.to_int max_used - List.length formals in
  if 0 <= extra + 1 then
    List.init (extra + 1) (fun _ -> I32Type)
  else
    []


module W = Wasm.Ast
module T = Wasm.Types
module V = Wasm.Values
open Wasm.Source

let value_type_conv _ : T.value_type = T.I32Type
let func_type_conv (s: func_type) : T.func_type =
  match s with
  | FuncType (s1, s2) -> T.FuncType (List.map value_type_conv s1, List.map value_type_conv s2) 
  

let type_conv (s: type_) : W.type_ = (func_type_conv s) @@ no_region

(*
let block_type_conv (t: block_type) : W.block_type =
  match t with
  | VarBlockType v -> W.VarBlockType (v @@ no_region)
  | ValBlockType v -> W.ValBlockType (Option.map value_type_conv v)
*)

let value_conv (v: value) : V.value =
  match v with
  | I32 i -> V.I32 i


let testop_conv (t: testop) : W.testop =
  match t with
  | I32 o ->
    match o with
    | I32Op.Eqz -> V.I32 (W.I32Op.Eqz)

let relop_conv (t: relop) : W.relop =
  match t with
  | I32 o ->
    match o with
    | I32Op.Eq -> V.I32 (W.I32Op.Eq)
    | I32Op.Ne -> V.I32 (W.I32Op.Ne)
    | I32Op.LtS -> V.I32 (W.I32Op.LtS)
    | I32Op.LtU -> V.I32 (W.I32Op.LtU)
    | I32Op.GtS -> V.I32 (W.I32Op.GtS)
    | I32Op.GtU -> V.I32 (W.I32Op.GtU)
    | I32Op.LeS -> V.I32 (W.I32Op.LeS)
    | I32Op.LeU -> V.I32 (W.I32Op.LeU)
    | I32Op.GeS -> V.I32 (W.I32Op.GeS)
    | I32Op.GeU -> V.I32 (W.I32Op.GeU)

let binop_conv (t: binop) : W.binop =
  match t with
  | I32 o ->
    match o with
    | I32Op.Add -> V.I32 (W.I32Op.Add)
    | I32Op.Sub -> V.I32 (W.I32Op.Sub)
    | I32Op.Mul -> V.I32 (W.I32Op.Mul)
    | I32Op.DivS -> V.I32 (W.I32Op.DivS)
    | I32Op.DivU -> V.I32 (W.I32Op.DivU)
    | I32Op.RemS -> V.I32 (W.I32Op.RemS)
    | I32Op.RemU -> V.I32 (W.I32Op.RemU)
    | I32Op.And -> V.I32 (W.I32Op.And)
    | I32Op.Or -> V.I32 (W.I32Op.Or)
    | I32Op.Xor -> V.I32 (W.I32Op.Xor)
    | I32Op.Shl -> V.I32 (W.I32Op.Shl)
    | I32Op.ShrS -> V.I32 (W.I32Op.ShrS)
    | I32Op.ShrU -> V.I32 (W.I32Op.ShrU)
    | I32Op.Rotl -> V.I32 (W.I32Op.Rotl)
    | I32Op.Rotr -> V.I32 (W.I32Op.Rotr)

let unop_conv (t: unop) : W.unop =
  match t with
  | I32 o ->
    match o with
    | I32Op.Clz -> V.I32 (W.I32Op.Clz)
    | I32Op.Ctz -> V.I32 (W.I32Op.Ctz)
    | I32Op.Popcnt -> V.I32 (W.I32Op.Popcnt)

let dummy_bt = W.ValBlockType None

let rec instr'_conv (i: instr) : W.instr' =
  match i with
  | Unreachable -> W.Unreachable
  | Nop -> W.Nop
  | Drop -> W.Drop
  | Select -> W.Select 
  | Block instrs ->
    W.Block (dummy_bt, List.map instr_conv instrs)
  | Loop instrs ->
    W.Loop (dummy_bt, List.map instr_conv instrs)
  | If (instrs1, instrs2) ->
    W.If (dummy_bt,
          List.map instr_conv instrs1,
          List.map instr_conv instrs2)
  | Br v -> W.Br (v @@ no_region)
  | BrIf v -> W.BrIf (v @@ no_region)
  | Return -> W.Return
  | Call v -> W.Call (v @@ no_region)
  | LocalGet v -> W.LocalGet (v @@ no_region)
  | LocalSet v -> W.LocalSet (v @@ no_region)
  | LocalTee v -> W.LocalTee (v @@ no_region)
  | Const n -> W.Const (value_conv n @@ no_region)
  | Test t -> W.Test (testop_conv t)
  | Compare r -> W.Compare (relop_conv r)
  | Unary u -> W.Unary (unop_conv u)
  | Binary b -> W.Binary (binop_conv b)
and instr_conv i = instr'_conv i @@ no_region    
  
let rec func'_conv (f: func) : W.func' =
  { W.ftype = f.ftype @@ no_region;
    W.locals = List.map value_type_conv f.locals;
    W.body = List.map instr_conv f.body
  }
and func_conv f = func'_conv f @@ no_region

let rec export_desc'_conv (d: export_desc) : W.export_desc' =
  match d with
  | FuncExport v -> W.FuncExport (v @@ no_region)
  | TableExport v -> W.TableExport (v @@ no_region)
  | MemoryExport v -> W.MemoryExport (v @@ no_region)
  | GlobalExport v -> W.GlobalExport (v @@ no_region)
and export_desc_conv d : W.export_desc = export_desc'_conv d @@ no_region

let export_conv (e: export) : W.export =
  { W.name = e.name ;
    W.edesc = export_desc_conv (e.edesc) } @@ no_region
 
let print_import =
  { W.module_name = List.map Char.code (Lib.explode "spectest");
    W.item_name = List.map Char.code (Lib.explode "print_i32");
    W.idesc = W.FuncImport (Int32.zero @@ no_region) @@ no_region }

let rec module_conv' (m: module_) : W.module_' =
  { W.types = List.map type_conv (m.types);
    W.globals = [];
    W.tables = [];
    W.memories = [];
    W.funcs = List.map (fun f -> func'_conv f @@ no_region) (m.funcs);
    W.start = Option.map (fun i -> i @@ no_region) m.start;
    W.elems = [];
    W.data = [];
    W.imports = [print_import @@ no_region];
    W.exports = List.map export_conv (m.exports);
  }
and module_conv (m: module_) : W.module_ =  module_conv' m @@ no_region

let print_module (m: module_ ) =
  Wasm.Print.module_ stdout 80 (module_conv m)

let output_module ch (m: module_ ) =
  Wasm.Print.module_ ch 80 (module_conv m)

let module_to_string (m: module_ ) =
  Wasm.Sexpr.to_string 80 (Wasm.Arrange.module_ (module_conv m))

(* Dummy host module to give a print function *)


let host_print_func : _ Wasm.Func.t =
  Wasm.Func.HostFunc (T.FuncType ([T.I32Type], []),
                      fun vs -> let _ = print_string (Wasm.Values.string_of_values vs) in [])

let host_print_extern : Wasm.Instance.extern =
  Wasm.Instance.ExternFunc host_print_func
    
let func f t = Wasm.Func.alloc_host t (f t)

let print_value v =
  Printf.printf "%s : %s\n"
    (Wasm.Values.string_of_value v) (Wasm.Types.string_of_value_type (Wasm.Values.type_of v))

let print (T.FuncType (_, out)) vs =
  List.iter print_value vs;
  flush_all ();
  List.map V.default_value out

let lookup name t =
  match Wasm.Utf8.encode name, t with
  | "print", _ -> Wasm.Instance.ExternFunc (func print (T.FuncType ([T.I32Type], [])))
  | "print_i32", _ -> Wasm.Instance.ExternFunc (func print (T.FuncType ([T.I32Type], [])))
  | _ -> raise Not_found

let configure ()  = Wasm.Import.register (Wasm.Utf8.decode "host") lookup
