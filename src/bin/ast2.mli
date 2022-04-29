(* file: ast.mli
   author: Bob Muller

   CSCI 3366 Principles of Programming Languages
*)
type program = Program of procedure list [@@deriving sexp]

and procedure = Procedure of { id : Symbol.t
                             ; formals : Symbol.binding list
                             ; typ : Typ.t
                             ; body : statement
                             }

and statement = Block of { decls : Symbol.binding list
                         ; statements : statement list
                         }
              | Assign of { id : Symbol.t
                          ; expr : term
                          }
              | While of { expr : term
                         ; statement : statement
                         }
              | IfS of { expr : term
                       ; thn : statement
                       ; els : statement
                       }
              | Call of { rator : Symbol.t
                        ; rands : term list
                        }
              | Print of term
              | Return of term

and term = Id of Symbol.t
         | Literal of { typ : Typ.t
                      ; bits : int
                      }
         | App of { rator : Symbol.t
                  ; rands : Symbol.t list
                  }
         | Let of { decl : declaration
                  ; body : term
                  }
and
  declaration = ValBind of { bv : Symbol.binding
                           ; defn : term
                           }

and
  value = LiteralValue of { typ : Typ.t
                          ; bits : int
                          }
        | BinaryOp of (value * value -> value)
        | UnaryOp of (value -> value)

val toStringList : 'a list -> ('a -> string) -> string
val toString : term -> string
val toStringValue : value -> string

val pp : program -> unit
val ppv : value -> unit
val ppStream : out_channel -> program -> unit
val ppFile : string -> program -> unit
val setIndentLevel : int -> unit
