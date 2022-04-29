(*
 * file: Lift.sml
 * author: Bob Muller
 * date: 1-1-2009.
 *
 * The Lift module implements a source-to-source transformation on
 * the nested let-expressions that may (or may not) have been introduced
 * in the naming phase. The transformation rule lifts inner let
 * expressions out. The transformation rule is:
 *
 * let x1 = (let x2 = e2 in e3) in e4
 *
 * is replaced by:
 *
 * let x2 = e2 in (let x1 = e3 in e4).
 *
 * Note that e2 may be a let-expression so the process iterates until
 * all let-expressions are lifted to top-level.
 *)

let rec translate (Ast2.Program procedures) =
  Ast3.Program (List.map translateProcedure procedures)

and
  translateProcedure (Ast2.Procedure {id; formals; typ; body}) =
  Ast3.Procedure { id
                ; formals
                ; typ
                ; body = translateStatement body
                }
and
  translateStatement statement = match statement with 
    |Ast2.Block {decls; statements;} -> Ast3.Block {decls = decls; statements = List.map translateStatement statements;}
    |Ast2.Assign {id; expr;} -> Ast3.Assign {id = id; expr = translateTerm expr;}
    |Ast2.While {expr;statement;} -> Ast3.While {expr = translateTerm expr; statement = translateStatement statement;}
    |Ast2.IfS{expr; thn; els;} -> Ast3.IfS{expr = translateTerm expr; thn = translateStatement thn; els = translateStatement els;}
    |Ast2.Call{rator; rands;} -> Ast3.Call{rator = rator; rands = List.map translateTerm rands;}
    |Ast2.Print a -> Ast3.Print (translateTerm a)
    |Ast2.Return a -> Ast3.Return (translateTerm a)

and

translateTerm term = match term with
  |Ast2.Let {decl = ValBind {bv; defn;}; body;} -> (let bv1 = bv in let bo1 = body in let def1 = defn in match def1 with
    |Ast2.Let {decl = ValBind {bv; defn;}; body;} -> Ast3.Let {decl = ValBind {bv = bv; defn = term2factor defn;}; body = translateTerm (Ast2.Let {decl = ValBind {bv = bv1; defn = body;}; body = bo1;});}
    |_ -> Ast3.Let {decl = ValBind {bv = bv; defn = (term2factor defn);}; body = (translateTerm body);})
  |_ -> term2term term

and

term2factor term = match term with
  |Ast2.Id a -> Ast3.Id a
  |Ast2.Literal {typ; bits;} -> Ast3.Literal {typ = typ; bits = bits;}
  |Ast2.App {rator; rands;} -> Ast3.App {rator = rator; rands = rands;}
  |_ -> Ast3.Literal {typ = Typ.Int; bits = 0;}

and

term2term term = match term with
  |Ast2.Let _ -> failwith ("no")
  |Ast2.Id a -> Ast3.Factor (Ast3.Id a)
  |Ast2.Literal {typ; bits;} -> Ast3.Factor (Ast3.Literal {typ = typ; bits = bits;})
  |Ast2.App {rator; rands;} -> Ast3.Factor (Ast3.App {rator = rator; rands = rands;})