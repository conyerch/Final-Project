/* file: parser.mly */

%{
  let mkSym(s) = Symbol.fromString s
  let mkId s = Ast1.Id(mkSym s)

  let mkType string =
    match string with
    | "int"  -> Typ.Int
    | "bool" -> Typ.Bool
    | "void" -> Typ.Void
    | _ -> raise(Failure "lexer: bad type")
let mkApp(x, rands) = Ast1.App {rator = mkSym x; rands = rands}
let mkInt n = Ast1.Literal {typ = Typ.Int; bits = n}
let mkVoid () = Ast1.Literal {typ = Typ.Void; bits = 0}
let mkBool n = Ast1.Literal {typ = Typ.Bool; bits = n}
let mkBinding id typ = {Symbol.id = mkSym id; typ = mkType typ}
let mkAssn lhs rhs = Ast1.Assign {id = mkSym lhs; expr = rhs}
let mkProc typ id formals body =
  Ast1.Procedure {id = mkSym id;
                 formals = formals;
                 typ = mkType typ;
                 body = body}
let mkCall rator rands = Ast1.Call {rator = mkSym rator; rands = rands}
let mkWhile expr stmt = Ast1.While {expr = expr; statement = stmt}
let mkIfS expr thn els = Ast1.IfS {expr = expr; thn = thn; els = els}
let mkBlk decls stmts = Ast1.Block {decls = decls; statements = stmts}
%}

%token <int> NUM
%token <string> ID
%token <string> TYPE
%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE UNIT COLON
%token TRUE FALSE IF THEN ELSE AND OR NOT PRINT WHILE RETURN
%token PLUS MINUS MULTIPLY DIVIDE MOD CARET LT LE EQ NE NEALT GE GT GETS QUESTION
%token EOF

%nonassoc IF THEN ELSE COLON QUESTION
%left OR
%left AND
%left LT LE EQ NE NEALT GE GT
%left PLUS MINUS
%left MULTIPLY DIVIDE MOD SEMI
%right CARET
%nonassoc NOT TRUE FALSE LPAREN LBRACE RBRACE

%start program
%type <Ast1.program> program

%% /* Grammar rules and actions follow */

program: procedures EOF                            { Ast1.Program($1) }
;
procedures: procedure                              { [$1] }
  | procedures procedure                           { $1 @ [$2] }
  ;

procedure: TYPE ID LPAREN parameters RPAREN block  { mkProc $1 $2 $4 $6 }
procedure: TYPE ID UNIT block  		/* FIX */				 { mkProc $1 $2 [] $4 }
;

parameters: /* empty */                          		 { [] }
  |	declaration                                      { [$1] }
  | declaration COMMA parameters                     { $1::$3 }
;

statement:
  RETURN exp SEMI                                  { Ast1.Return($2) }
  | ID LPAREN expressions RPAREN SEMI              { mkCall $1 $3 }
  | ID UNIT SEMI              /* FIX */						 { mkCall $1 [] }
  | ID GETS exp SEMI                               { mkAssn $1 $3 }
  | PRINT exp SEMI                                 { Ast1.Print($2) }
  | block                                          { $1 }
  | WHILE LPAREN exp RPAREN statement              { mkWhile $3 $5 }
  | IF LPAREN exp RPAREN statement ELSE statement  { mkIfS $3 $5 $7}
;

block: LBRACE declarations statements RBRACE       { mkBlk $2 $3 }
block: LBRACE statements RBRACE                    { mkBlk [] $2 }
;

declarations: declaration SEMI                     { [$1] }
  | declarations declaration SEMI                  { $1 @ [$2] }
;

declaration: TYPE ID                               { mkBinding $2 $1 }
;

statements: statement                              { [$1] }
  | statements statement                           { $1 @ [$2] }
;

expressions:  /* empty */                          { [] }
  | exp                                            { [$1] }
  | exp COMMA expressions                          { $1::$3 }
;
exp: ID                                            { mkId($1) }
  | NUM                                            { mkInt $1 }
  | UNIT                                            { mkVoid () }
  | TRUE                                           { mkBool 1 }
  | FALSE                                          { mkBool 0 }
  | exp PLUS exp                                   { mkApp("+", [$1; $3]) }
  | exp MINUS exp                                  { mkApp("-", [$1; $3]) }
  | exp MULTIPLY exp                               { mkApp("*", [$1; $3]) }
  | exp DIVIDE exp                                 { mkApp("/", [$1; $3]) }
  | exp CARET exp                                  { mkApp("**", [$1; $3]) }
  | exp MOD exp                                    { mkApp("%", [$1; $3]) }
  | exp LT exp                                     { mkApp("<", [$1; $3]) }
  | exp LE exp                                     { mkApp("<=", [$1; $3]) }
  | exp EQ exp                                     { mkApp("==", [$1; $3]) }
  | exp NE exp                                     { mkApp("<>", [$1; $3]) }
  | exp NEALT exp                                  { mkApp("<>", [$1; $3]) }
  | exp GT exp                                     { mkApp(">", [$1; $3])  }
  | exp GE exp                                     { mkApp(">=", [$1; $3]) }
  | NOT exp                                        { mkApp("not", [$2]) }
  | MINUS exp                                      { mkApp("-",[mkInt 0;$2]) }
  | LPAREN exp RPAREN                              { $2 }
  | ID LPAREN expressions RPAREN                   { mkApp($1, $3) }
  | ID UNIT										/* FIX */            { mkApp($1, []) }
  | exp AND exp                                    { mkApp("and", [$1; $3]) }
  | exp OR exp                                     { mkApp("or", [$1; $3]) }
  ;
%%
