# CSCI 3366 Programming Languages

##### See the `README.md` file in part 1 for a full description of the miniC compiler project.

## IMPORTANT INSTRUCTIONS ##

You **MUST** install the WebAssembly opam package in order to compile and run this assignment. Please run the following commands from the terminal:

```bash
opam install wasm.1.1.1
```
---

### Part 3: (28 Points): Codegen --- due Thursday May 5, 2022, 6PM


The codegen phase of the compiler is implemented by the translation

```
Codegen.translate : Ast3.program -> Astwasm.module_
```

The harness code for the `Codegen` module is found in file `codegen.ml`. This is the last phase of the compiler, in which we convert from our intermediate `Ast3` representation into WebAssembly.

You can test your code by running

```bash
> dune exec bin/main.exe try codegen
```

This will run your codegen transformation on the code produced by the lifting phase of the reference compiler and run the resulting WebAssembly to see whether it produces the correct output.

Optionally, you may copy over your `name.ml` and `lift.ml` solutions to PS7 and PS8 in addition to the `bin/` directory. If you do so, and run

```bash
>  dune exec bin/main.exe test/a.mc
```

where `a.mc` is the name of a miniC file from the `test/` directory, your compiler will produce the full Web Assembly text format output in `test/a.wat`.

---

