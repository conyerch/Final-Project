(* file: try.ml

   This is a simple system for trying out various compiler phases
   when the earlier phases aren't finished.

*)

(************************************************************)

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

open Sexplib

let name_tests =  [  "test/a.mc"
                   ; "test/b.mc"
                   ; "test/basic_ops.mc"
                   ; "test/c.mc"
                   ; "test/cond1.mc"
                   ; "test/cond2.mc"
                   ; "test/cond2_inv.mc"
                   ; "test/d.mc"
                   ; "test/e.mc"
                   ; "test/f.mc"
                   ; "test/fact.mc"
                   ; "test/g.mc"
                   ; "test/gcd.mc"
                   ; "test/t1.mc"
                   ; "test/threefacts.mc"
                   ; "test/totient.mc"
                   ]

let dumpName1 rt fileName =
  let _ = Printf.printf "Dumping %s\n" fileName in
  let out = Util.makeFile (rt ^ fileName, "ast2") in
  let pgm = parseFile fileName in
  let named = Name.translate pgm in
  let sexp = Ast2.sexp_of_program named in
  let _ =
    match Ast2.program_of_sexp sexp = named with
    | false -> failwith "nomatch"
    | true -> ()
  in
  let _ = output_string out (Sexp.to_string sexp) in
  close_out out

let tryName1 fileName =
  let _ = Printf.printf "Testing %s" fileName in
  let ans_sexp = input_line (open_in (Util.makeFileName (fileName, "ast2"))) in
  let ans = Ast2.program_of_sexp (Sexp.of_string ans_sexp) in
  let pgm = parseFile fileName in
  let named = Name.translate pgm in
  (if Equiv.testEquivProgram ans named then
     Printf.printf "-- passed!\n"
   else
     (Printf.printf "-- failed!\n";
      Printf.printf "==========================\n";
      Printf.printf "Before the name phase:\n";
      Ast1.pp pgm;
      Printf.printf "\n\nAfter the name phase:\n";
      Ast2.pp named;
      Printf.printf "\n\nReference solution:\n";
      Ast2.pp ans;
      Printf.printf "==========================\n";
     )
  )


let configure buf () =
  Wasm.Import.register (Wasm.Utf8.decode "spectest") (Hostenv.lookup buf)

let run_wasm1 m =
  let buf = Buffer.create 0 in
  configure buf ();
  let module_str = Astwasm.module_to_string m ^ "(invoke \"main\")"
  in
  let _ = Wasm.Run.run_string module_str in
  let _ = print_string (Buffer.contents buf) in
  Buffer.contents buf
    
let run_wasm_postconfig buf m =
  let _ = Buffer.clear buf in
  let module_str = Astwasm.module_to_string m ^ "(invoke \"main\")"
  in
  let _ = Wasm.Run.run_string module_str in
  Buffer.contents buf

let rec loop_tests f tests =
  match tests with
  | [] -> ()
  | name :: tests ->
    let _ = f name in
    loop_tests f tests

let dumpName rt = loop_tests (dumpName1 rt) name_tests
let tryName () = loop_tests tryName1 name_tests

let dumpCodegen1 rt fileName =
  let _ = Printf.printf "Dumping %s\n" fileName in
  let lifted_sexp = input_line (open_in (Util.makeFileName (fileName, "ast3"))) in
  let lifted = Ast3.program_of_sexp (Sexp.of_string lifted_sexp) in
  let out = Util.makeFile (rt ^ fileName, "wat") in
  let wasm = Codegen.translate lifted in
  let _ = Astwasm.output_module out wasm in
  close_out out

let dumpRun1 runner rt fileName =
  let _ = Printf.printf "Dumping %s\n" fileName in
  let lifted_sexp = input_line (open_in (Util.makeFileName (fileName, "ast3"))) in
  let lifted = Ast3.program_of_sexp (Sexp.of_string lifted_sexp) in
  let out = Util.makeFile (rt ^ fileName, "res") in
  let _ = try
      let wasm = Codegen.translate lifted in
      let res = runner wasm in
      output_string out res
      with _ -> output_string out "ERROR" in
  close_out out

let rec readFromChannel inch letters =
  try
    let line  = input_line inch ^ "\n" in
    readFromChannel inch (letters ^ line)
  with
    End_of_file -> close_in inch ;
    letters

let load_res filename =
  let inch = open_in filename in
  let str = readFromChannel inch ""
  in str

let tryCodegen1 runner fileName =
  let _ = Printf.printf "\n\nTesting %s" fileName in
  let lifted_sexp = input_line (open_in (Util.makeFileName (fileName, "ast3"))) in
  let lifted = Ast3.program_of_sexp (Sexp.of_string lifted_sexp) in
  let wasm = Codegen.translate lifted in
  let ans_res = load_res (Util.makeFileName (fileName, "res")) in
  let res = runner wasm in
  if ans_res = res then
    Printf.printf "-- passed!\n"
  else
     (Printf.printf "-- failed!\n";
      Printf.printf "==========================\n";
      Printf.printf "Before the codegen phase:\n";
      Ast3.pp lifted;
      Printf.printf "\n\nAfter the codegen phase:\n";
      Astwasm.print_module wasm;
      Printf.printf "\n\nRuning main() produced:\n";
      print_string res;
      Printf.printf "\n\nRuning main() in reference solution produced:\n";
      print_string ans_res;
      Printf.printf "==========================\n";
     )

let dumpCodegen rt = loop_tests (dumpCodegen1 rt) name_tests
let dumpRun rt =
  let buf = Buffer.create 0 in
  let _ = configure buf () in
  loop_tests (dumpRun1 (run_wasm_postconfig buf) rt) name_tests

let tryCodegen () =
  let buf = Buffer.create 0 in
  let _ = configure buf () in
  loop_tests (tryCodegen1 (run_wasm_postconfig buf)) name_tests
