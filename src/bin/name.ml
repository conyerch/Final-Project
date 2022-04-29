(* file: name.ml
 * author: Bob Muller
 * date: January 5, 2009
 * revised: March, 2017
 *
 * The Name module implements a source-to-source transformation for
 * naming the values of subterms in miniC. In addition to naming values,
 * the Name module translates "or" and "and" forms into special-cases
 * of the conditional form nested within a let-form.
*)
let rec translate (Ast1.Program procedures) =
  Ast2.Program (List.map translateProcedure procedures)

and
  translateProcedure (Ast1.Procedure {id; formals; typ; body}) =
  Ast2.Procedure { id
                ; formals
                ; typ
                ; body = translateStatement body
                }
and
  translateStatement statement = match statement with 
    |Ast1.Block {decls; statements;} -> Ast2.Block {decls = decls; statements = List.map translateStatement statements;}
    |Ast1.Assign {id; expr;} -> Ast2.Assign {id = id; expr = translateTerm expr;}
    |Ast1.While {expr;statement;} -> Ast2.While {expr = translateTerm expr; statement = translateStatement statement;}
    |Ast1.IfS{expr; thn; els;} -> Ast2.IfS{expr = translateTerm expr; thn = translateStatement thn; els = translateStatement els;}
    |Ast1.Call{rator; rands;} -> Ast2.Call{rator = rator; rands = List.map translateTerm rands;}
    |Ast1.Print a -> Ast2.Print (translateTerm a)
    |Ast1.Return a -> Ast2.Return (translateTerm a)

and
  translateTerm term =
  match term with
  | Ast1.Literal {bits; typ;} -> Ast2.Literal {bits = bits; typ = typ;};
  | Ast1.App {rator; rands;} -> translateApp (Ast1.App {rator; rands;})
  |Ast1.Id a -> Ast2.Id a

and 

  translateApp app = match app with
  |Ast1.App {rator; rands;} -> (match rands with
    |h::ls'::_ -> let x = Symbol.fresh() in let y = Symbol.fresh() in let z = Symbol.fresh() in Ast2.Let {decl = ValBind {bv = {id = x; typ = Typ.Int;}; defn = translateApp h;}; body = Ast2.Let {decl = ValBind {bv = {id = y; typ = Typ.Int;}; defn = translateApp ls';}; body = Ast2.Let {decl = ValBind {bv = {id = z; typ = Typ.Int;}; defn = Ast2.App {rator = rator; rands = [x;y;];}}; body = Ast2.Id z}}}
    |h::_ -> let y = Symbol.fresh() in let z = Symbol.fresh() in Ast2.Let {decl = ValBind {bv = {id = y; typ = Typ.Int;}; defn = translateApp h;}; body = Ast2.Let {decl = ValBind {bv = {id = z; typ = Typ.Int;}; defn = Ast2.App {rator = rator; rands = [y;];}}; body = Ast2.Id z}}
    |_ -> failwith ("error"))
  |Ast1.Literal {bits; typ;} -> Ast2.Literal {bits = bits; typ = typ;};
  |Ast1.Id a -> Ast2.Id a
