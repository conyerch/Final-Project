(* file: ast1.ml
   author: Bob Muller

   CSCI 3366 Principles of Programming Languages

  This file contains an abstract syntax for the programming language miniC.
  The abstract syntax includes a few items that aren't used directly in
  miniC but (some of which) are used in transformations on miniC programs.
 *)

type program = Program of procedure list

and procedure = Procedure of { id : Symbol.t
                             ; formals : Symbol.binding list
                             ; typ : Typ.t
                             ; body : statement
                             }

and statement = Block of { decls : Symbol. binding list
                         ; statements : statement list
                         }
              | Assign of { id : Symbol.t
                          ; expr : term
                          }
              | While of { expr : term
                         ; statement : statement
                         }
              | IfS of { expr : term
                       ; thn  : statement
                       ; els  : statement
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
                  ; rands : term list
                  }
and
  value = LiteralValue of { typ : Typ.t
                          ; bits : int
                          }
        | BinaryOp of (value * value -> value)
        | UnaryOp of (value -> value)

(********************************************************************
  *
  * toString utilities.
  *
  * The following is a utility function for making a list of comma-separated
  * strings where the items separated are arbitrary.
*)
let rec toStringList items toStringer =
  match items with
  |	[] -> ""
  | [only] -> toStringer only
  | item :: items ->
    let itms = toStringer item in
    let itmss = toStringList items toStringer
    in
    Lib.fmt "%s, %s" itms itmss


(* make a string for an assignment: M = N
*
let toStringEqual s1 s2 = Lib.fmt "%s = %s" s1 s2
*)
(* The toString function on terms.
*)
let rec toString term =
  match term with
  | Id name -> Symbol.format name

  | Literal {typ = Typ.Int; bits} -> string_of_int bits
  | Literal {typ = Typ.Bool; bits = 0} -> "false"
  | Literal {typ = Typ.Bool; bits = 1} -> "true"
  | Literal _ -> failwith "Ast.toString: this shouldn't happen"

  | App {rator; rands} ->
    let ras = Symbol.format rator in
    let commaSepRands = toStringList rands toString
    in
    Lib.fmt "%s(%s)" ras commaSepRands

(* toStringValue : value -> string
*)
let toStringValue value =
  match value with
  | LiteralValue {typ = Typ.Int; bits} -> (string_of_int bits)
  | LiteralValue {typ = Typ.Bool; bits = 1} -> "true"
  | LiteralValue {typ = Typ.Bool; bits = 0} -> "false"
  | _ -> "unknown value";;

(* PRETTY
*)
let ppIndentLevel = ref 2

let setIndentLevel level = ppIndentLevel := level

(* Pretty print a value. All of this pretty-printing code should be
  replaced with code using Ocaml's Format module.
*)
let ppValue out indent value =
  Util.writeln(out, indent, toStringValue value)

let rec ppProgram out indent (Program procedures) =
  List.iter (fun proc -> ppProcedure out indent proc) procedures

and ppProcedure out indent (Procedure {id; formals; typ; body}) =
  let fp = fun b -> (Typ.format (b.Symbol.typ)) ^ " " ^ (Symbol.format b.Symbol.id) in
  let fs = "(" ^ (toStringList formals fp) ^ ")" in
  let ns = (Typ.format typ) ^ " " ^ (Symbol.format id)
  in
  Util.writeln(out, indent, ns ^ fs)
  ; ppBlock out indent body

and ppBlock out indent body =
  match body with
  | Block {decls; statements} ->
    let nd = indent + (!ppIndentLevel) in
    let pdecs = fun {Symbol.id; Symbol.typ} ->
      Util.writeln(out, nd, Lib.fmt "%s %s;" (Typ.format typ) (Symbol.format id))
    in
    Util.writeln(out, indent, "{")
    ; List.iter pdecs decls
	  ; List.iter (fun statement -> ppStatement out nd statement) statements
    ; Util.writeln(out, indent, "}")
  | _ -> raise (Failure "ppBlock: cannot happen.")

and ppStatement out indent statement =
  match statement with
  | Block _ -> ppBlock out indent statement

  | Assign {id; expr} ->
    let ids = Symbol.format id in
    let exprs = toString expr
    in
    if String.length exprs > 20 then
      (
        Util.write(out, indent, Lib.fmt "%s = " ids);
        ppTerm out indent expr;
        Util.writeln(out, indent, ";")
      )
    else
      Util.writeln(out, indent, Lib.fmt "%s = %s;" ids exprs)

  | While {expr; statement} ->
    let ts = toString expr
    in
    (
      Util.writeln(out, indent, Lib.fmt "while (%s)" ts);
      ppStatement out (indent + (!ppIndentLevel)) statement
    )

  | IfS {expr; thn; els} ->
    let exprs = toString expr
    in
    (
      if String.length exprs > 20 then
        (
          Util.write(out, indent, "if (");
          ppTerm out indent expr;
          Util.writeln(out, indent, ") then")
        )
      else
        Util.writeln(out, indent, Lib.fmt "if (%s) then" exprs)
    );
    ppStatement out (indent + (!ppIndentLevel)) thn;
    Util.writeln(out, indent, "else");
    ppStatement out (indent + (!ppIndentLevel)) els

  | Call {rator; rands} ->
    let rators = Symbol.format rator in
    let calls = Lib.fmt "call %s(%s);" rators (toStringList rands toString)
    in
    Util.writeln(out, indent, calls)

  | Print expr ->
    let exprs = toString expr
    in
    Util.writeln(out, indent, Lib.fmt "print %s;" exprs)

  | Return expr ->
    (
      Util.writeln(out, indent, "return");
      ppTerm out (indent + !ppIndentLevel) expr
    )

(* Pretty print a term.
*)
and ppTerm out indent term =
  match term with
  | Id _ | Literal _ -> Util.writeln(out, indent, toString term)

  | App {rator; rands} ->
    let rators = Symbol.format rator in
    let randss = toStringList rands toString
    in
    Util.writeln(out, indent, Lib.fmt "%s(%s)" rators randss)

let ppStream out pgm = ppProgram out 0 pgm

let pp = ppStream stdout

let ppv value = ppValue stdout 0 value

let ppFile fileName term =
  let out = open_out fileName
  in
  (
    ppStream out term;
	  close_out out
	)
