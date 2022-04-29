module Q = Quads
module A = Ast
(* name: operation and function calls only on variables *)
(* lift: no nested lets *)


(*
let getLabel maybeLabel =
  match maybeLabel with
  | None -> Label.fresh()
  | Some label -> label

type transExprOut = { instructions : Q.instruction list
                    ; result : Q.rhs
                    }

let mark label = [Q.Instruction {label = Some label; op = Q.Noop}]
let jump label = [Q.Instruction {label = None; op = Q.Jmp label}]

let terms2opnds terms =
  List.map
    (fun term ->
       match term with
       | A.Id sym -> Q.Id sym
       | _ -> failwith "Control.terms2opnds: Should not happen"
    )
    terms

let invoke rator terms =
  let name = Label.fromString (Symbol.format rator)
  in
  { instructions = []
  ; result = Q.FunCall { label = name
                       ; opnds = terms2opnds terms
                       }
  }

let initialize decls =
  List.map
    (fun A.{id; typ} ->
       match typ with
       | Typ.Int ->
         [ Q.Instruction { label = None
                         ; op = Q.Gets { dst = Q.Id id
                                       ; src = Q.Operand (Q.Word { typ
                                                                 ; bits = 0
                                                                 })
                                       }
                         }
         ]
       | Typ.Bool ->
         [ Q.Instruction { label = None
                         ; op = Q.Gets { dst = Q.Id id
                                       ; src = Q.Operand (Q.Word { typ
                                                                 ; bits = 0
                                                                 })
                                       }
                         }
         ]
       | _ -> [])
    decls
*)
let translate _ = Q.Program [] (* YOUR CODE HERE *)
