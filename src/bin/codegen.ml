module W = Astwasm

let getProcedureType (p: Ast3.procedure) : W.func_type =
  let Ast3.Procedure {id=_; formals; typ; body=_} = p in
  let res_type =
    match typ with
    | Typ.Void -> []
    | _ -> [W.I32Type] in
  let in_type = List.map (fun _ -> W.I32Type) formals in
  FuncType (in_type, res_type)

let getName (p: Ast3.procedure) : Symbol.t =
  let Ast3.Procedure {id; formals=_; typ=_; body=_} = p in id

let makeFuncDict ps : int32 Env.t =
  let ps' = List.mapi (fun i p -> (Int32.of_int (i + 1), getName p)) ps in
  List.fold_left (fun m (v, i) -> Env.add i v m) Env.empty ((Int32.zero, Symbol.fromString "p") :: ps')

(* This provides a mapping between operations in our MiniC language
   and corresponding WASM instructions *)
let operatorInstructions : (W.instr list) list =
  [   (* + *)   [W.Binary (I32 (W.I32Op.Add))]
    ; (* - *)   [W.Binary (I32 (W.I32Op.Sub))]
    ; (* * *)   [W.Binary (I32 (W.I32Op.Mul))]
    ; (* / *)   [W.Binary (I32 (W.I32Op.DivU))]
    ; (* % *)   [W.Binary (I32 (W.I32Op.RemU))]
    ; (* **  EXP OPERATOR NOT IMPLEMENTED *) []
    ; (* < *)   [W.Compare (I32 (W.I32Op.LtU))]
    ; (* <= *)  [W.Compare (I32 (W.I32Op.LeU))]
    ; (* == *)  [W.Compare (I32 (W.I32Op.Eq))]
    ; (* <> *)  [W.Compare (I32 (W.I32Op.Ne))]
    ; (* >= *)  [W.Compare (I32 (W.I32Op.GeU))]
    ; (* > *)   [W.Compare (I32 (W.I32Op.GtU))]
    ; (* not *) [W.Test (I32 (W.I32Op.Eqz))]
    ; (* and *) [W.Binary (I32 (W.I32Op.And))]
    ; (* or *)  [W.Binary (I32 (W.I32Op.Or))]
        ]

let dynamicBasis = Env.make operatorInstructions

(* Given a rator (an id for an operation/function), this returns a list of
   instructions to compute that operation: if the operation is a primitive operation,
   we use the instructions given above, otherwise the operation ought to be a function call.
   We look up its ID in the Env d, and return a Call instruction 

   HINT: you will want to use this in your code for `App` and `Call` *)
let funCodeGen d (id: Symbol.t) : W.instr list =
  (try
    Env.find id dynamicBasis
   with
     Not_found ->
     (try
        [W.Call (Env.find id d)]
      with Not_found -> failwith "bad function symbol"))

(* 
   HINT: for translateFactor
   - fenv is an Env that matches function symbol names to the number of the function in the WASM module.
     Use with funCodeGen
   - lenv is a Freshenv that matches variables to a corresponding WASM local
      variable number for the current function/block *)

let translateFactor (fenv : int32 Env.t) (lenv : Freshenv.t) (fact : Ast3.factor) : W.instr list =
  match fact with
  | Id s -> W.LocalGet (Freshenv.find s lenv) :: []
  | Literal { typ = _; bits } ->
        let newval = (Int32.of_int bits) in
        W.Const (I32 newval) :: []
  | App { rator; rands } ->
    let rec getinst rands =
      match rands with
      |[] -> []
      |x :: xs -> W.LocalGet (Freshenv.find x lenv) :: getinst xs
    in
    (getinst rands) @ (funCodeGen fenv rator)

(* HINT: in the Let case, use the function Freshenv.add1 to add the bound variable to lenv. This
   will associate fresh WASM local variable number with that symbol name *)
let rec translateTerm fenv lenv term : W.instr list =
  match term with
  | Ast3.Factor f -> translateFactor fenv lenv f
  | Ast3.Let { decl; body } ->
    match decl with
    | ValBind { bv; defn } -> (
        let lenv = Freshenv.add1 (bv.id) lenv in
        let inst1 = translateFactor fenv lenv defn in
        let inst2 = W.LocalSet (Freshenv.find bv.id lenv) :: [] in
        let inst3 = translateTerm fenv lenv body in
        let inst4 = inst1 @ inst2 in
        inst4 @ inst3)

(* HINT: the case for Block should help you with Ast3.Let in translateTerm if you are struggling *)
let rec translateStatement fenv lenv stmt : W.instr list =
  match stmt with
  | Ast3.Block { decls; statements } ->
      let lenv = Freshenv.extend (List.map (fun x -> x.Symbol.id) decls) lenv in
      List.concat (List.map (translateStatement fenv lenv) statements)
  | Ast3.Print term ->
    (* We import a special Print function to our WASM module which is assigned the number 0,
       so to print a term, we generate code to execute the term and store the result on the stack,
       then call the print function  *)
    translateTerm fenv lenv term @ [W.Call Int32.zero]
  (* FILL IN THE REMAINING CASES *)
  | Assign { id; expr } -> (
    let inst1 = translateTerm fenv lenv expr in
    let inst2 = W.LocalSet (Freshenv.find id lenv) :: [] in
    inst1 @ inst2)
  | While { expr; statement } -> (
    let inst1 = translateTerm fenv lenv expr in
    let inst2 = translateStatement fenv lenv statement in
    let inst2' = inst2 @ inst1 in
    let inst4 = W.Loop (inst2' @ (W.BrIf (Int32.zero) :: []) ) in
    let inst3 = W.If (inst4 :: [],  (Nop :: [])) :: [] in
    inst1 @ inst3)

  | IfS { expr; thn; els } ->
    (translateTerm fenv lenv expr) @ (W.If (translateStatement fenv lenv thn, translateStatement fenv lenv els) :: [])

  | Call { rator; rands } ->
    let rec getinst rands =
      match rands with
      |[] -> []
      |x :: xs -> (translateTerm fenv lenv x) @ (getinst xs)
    in
    List.append (getinst rands) (funCodeGen fenv rator)
  | Return term ->
    ((translateTerm fenv lenv term) @ (W.Return :: []))



(* ********************************** *)
(* NO CHANGES NEEDED BELOW THIS POINT *)
(* ********************************** *)

(* This code generates the appropriate type and local variable information for
   a wasm function based on the procedure p; the body of the function is generated
   by translating p's body through translateStatment
*)
let translateProcedure fenv p =
  match p with
  | Ast3.Procedure {id; formals; typ; body} ->
    let epilogue =
      match typ with
      | Void -> []
      | _ -> [W.Const (I32 (Int32.zero))]
    in
    let lenv = Freshenv.extend (List.map (fun x -> x.Symbol.id) formals) Freshenv.empty in
    let code = translateStatement fenv lenv body in
    { W.ftype = Env.find id fenv;
      W.locals = W.locals_types formals code; 
      W.body = code @ epilogue }

(* To finish translating the program, we translate each procedure and build up a module
   with the collected functions. We also need to assign numbers for each function and
   list their type, as well as import the externally supplied print function *)
let translate (Ast3.Program procedures)  =
  let tys = List.map (getProcedureType) procedures in
  let fenv = makeFuncDict procedures in 
  let _ = funCodeGen in
  let mainname = List.map Char.code (Lib.explode "main") in
  { W.types = FuncType ([W.I32Type], []) :: tys;
    W.funcs = List.map (translateProcedure fenv) procedures;
    W.start = None;
    W.exports = [{name = mainname;
                  edesc = FuncExport (Env.find (Symbol.fromString "main") fenv)}]
  }
