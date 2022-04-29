(* file: compile.ml
   author: R. Muller
   revised: February 2019

  The Compile module implements a gut-simple compiler for the language
  miniC.  The mc compiler generates code for the MIPS processor (as
  implemented by the MARS Simulator). The compiler consists of several
  major phases, each of which is implemented in a separate module.

   The following switch can be set via the command-line to run the
   type-checker.
*)
let typeChecking = ref true

(* compilerOptions processes compiler options and returns the
   position in Sys.argv where the source file ought to be. Remember
   that Sys.argv.(0) is the string showing the program invocation.
 *)
let compilerOptions() =
  match Array.length(Sys.argv) with
  | 2 -> 1
  |	3 ->
    (match Sys.argv.(1) with
     | "-nocheck" -> typeChecking := false; 2
     | "-t" -> Debug.debugLexer := true; 2
     | _ ->
       failwith (Lib.fmt "Unknow compiler option %s.\n\n" Sys.argv.(1)))
  | _ -> failwith "Too many compiler options.\n\n"

let parseFile fileName =
  let inch = open_in fileName in
  let lexbuf = Lexing.from_channel inch in
  let ast = (if !Debug.debugLexer then
               let _ = Debug.dumpTokens Lexer.token lexbuf
               in
               Ast1.Program([])
             else
               Parser.program Lexer.token lexbuf)
  in
  close_in inch ;
  ast

let compile() =
  let n = compilerOptions () in
  let filename = Sys.argv.(n) in
  let dbgOut = Debug.debugSetup filename in
  (*  let _ = Printf.printf "filename : [%s]\n" filename in *)

  (* Build the base type environment. *)
  let typeEnv = Env.make Staticbasis.operatorTypes in

  (* Parse the text in the input file. *)
  let ast = parseFile filename in
  let _ = Debug.debugInfo(dbgOut, "The input program is:", ast) in

  (* See if the user wants this program typed-checked. *)
  let _ = (if !typeChecking then
             let msg = (try
                          let _ = Static.typeCheck typeEnv ast
                          in
                          "\nThe program is well-typed.\n"
                        with Static.TypeError s ->
                          let _ = print_string s in
                          let _ = Util.writeln(dbgOut, 0, s)
                          in
                          failwith "Compilation failed.\n")
             in
             (if !Debug.debug then Util.writeln(dbgOut, 0, msg) else ())
           else
             (* No type checking, pretend the program was well-typed. *)
             ()) in

  (* Check for the presence of main function *)
  let Program ps = ast in
  let _ =
    match List.exists (fun (Ast1.Procedure {id; formals; _}) ->
        Symbol.format id = "main" && formals = []) ps with
    | true -> ()
    | false -> failwith "main() not found"
  in

  (* Perform the naming transformation to go from AST1 to AST2
  *)
  let named = Name.translate ast in
  let _ = Debug.debug2Info(dbgOut, "After the naming phase:", named) in

  (* Remove nested let-defintions, going from AST2 to AST3
  *)
  let lifted = Lift.translate named in
  let _ = Debug.debug3Info(dbgOut, "After the lifting phase:", lifted) in

  let copy = lifted in

  let m = Codegen.translate copy in
  let objf = Util.makeFile(filename, "wat") in
  let _ = Try.run_wasm1 m in
  let _ = Astwasm.output_module objf m in
  ()

(* The tryDispatch function is here to support
   student work on the compiler. Invoking the compiler
   as in

   > dune exec bin/main.exe -try lift

   e.g., will call the Lift.translate function on a
   fixed ast that would be the output of the Name
   transformation. It will pretty-print the result to
   the fixed file see.dbg.
*)
let tryDispatch () =
  match Sys.argv.(2) with
  | "name" -> Try.tryName ()
  | "codegen" -> Try.tryCodegen ()
  | _ -> failwith "test: unknown compiler phase."

let dumpDispatch () =
  match Sys.argv.(2) with
  | "name" -> Try.dumpName Sys.argv.(3)
  | "codegen" -> Try.dumpCodegen Sys.argv.(3)
  | "run" -> Try.dumpRun Sys.argv.(3)
  | _ -> failwith "test: unknown compiler phase."

let () =
  if Sys.argv.(1) = "try" then
    tryDispatch()
  else if Sys.argv.(1) = "dump" then
    dumpDispatch()
  else
    compile()

module T = Astwasm
module CG = Codegen
