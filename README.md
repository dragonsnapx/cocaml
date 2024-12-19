# Cocaml - A C compiler, built with Ocaml
Cocaml is a C compiler, leveraging Ocaml's functionality to translate and compile C.

## Installation

Cocaml requires llvm (>= 19) and clang to be installed. Depending on the environment you may need to manually link `llc` to $PATH as some package managers like homebrew do not link lld; For other distros, you may need to install `llc` (>= 20) manually and add to path. 

Due to limitations of the `LLVM` library, it does not run on macOS when installed with homebrew on ARM Macs.

## Usage
### As a library
Refer to `cocaml_main.mli` for the full spec. Cocaml allows user interaction from any part of the compilation process of (parsing/lexing -> translating -> compiling). `compile` compiles the program.

### As an executable
```
Usage: ./runner.exe [options] <filename>
 Options:
--compile              : Compiles the C file into an executable (default behavior).
--preprocess-only      : Outputs the preprocessed result of the input C file.
--to-llvm-ir           : Only outputs the LLVM IR.
--llvm                 : Compiles LLVM IR code to an executable.
--help                 : Show this help message.
```

## Structure

Cocaml compiles correct C code into LLVM IR, which then uses LLVM's `llc` to convert it assembly, then uses `clang` to compile to an object file.
```
(Runner ->) Main -> Lexer -> Parser -> Translator -> `llc` -> `clang` -> Executable
```

### Main (cocaml_main)
Main interface from cocaml, taking a .c file and has endpoints to return one of an AST, LLVM IR, or an executable. See `cocaml_main` for the full spec.

### Lexer
Given a set of "tokens" from `Menhir_parser`, the Lexer performs regex matching to extract these tokens from our given .c file (which is passed to the Lexer as a `Lexing.lexbuf` data structure). This can be seen with the `ocamllex lexer.mll` command (dune runs this command automatically during our compilation process). 

### Parser
Given a stream of tokens passed from the Lexer, the Parser checks this stream against a set of "grammar rules", and uses these to build an AST, according to the `Syntax_node` interface. This is the end of the front-end of the compiler -- the back-end simply receives an AST, which it interacts with via `Syntax_node`. At the top-level, our AST is a list of "declarations"

### Translator
Translates AST tree to LLVM IR (example available below) and returns any errors or syntactical issues via semantic analysis. The task of semantic analysis is made easier by directly using the OCaml `llvm` library, which allows us to interact with the translated code directly.

Some interesting facts about LLVM IR generation:
- LLVM calls `alloca` before all instruction sets - this means that even if a local variable appears much later down the line, it still is "allocated" at the beginning of the code block. 
- LLVM bindings with OCaml has a lot of "magic" behind; even though it is still very functional, it has elements such as the `module`, a mutable variable that acts as a file that is used to push instruction sets, which can be manipulated to output to an output stream or a file.
- It also does a lot of optimizations *pre* translation and *post* translation & compilation. For instance, declaring `int x = 3 + 4` generates the LLVM IR equivalent of `int x = 7`, and skips generating 

### Runner
Runs LLVM IR, generating an executable. This step will use LLVM itself, and has not been something we've worked on yet. 

## Challenges & Design Choices
- This was our first compiler, so there was a lot of background research into how compilers are structured and figuring out which would be the best choice. We landed on LLVM because it meant we could get closer to performance to GCC/Clang, and did not have to worry too much about memory alignment, register allocation and portability.
- Thanks to using LLVM, we were able to get very similar performance to GCC/Clang at times, for arithmetic (when both compiled with `-O2`, int & float).
- The translator was extremly hard to debug and test for various reasons:
    - The LLVM bindings for Ocaml are not purely functional & has some mutation in the background, making it very hard to debug.
    - Error messages that are cryptic (i.e. Exited with -10) and fails silently at times
    - Testing was done using lldb mostly, looking at registers/stack frame & valgrind to check if program is valid.
    - Comparing LLVM IR/Assembly was impossible since using `clang` to compile C code into LLVM IR and our compiler **yielded different assembly/IR but functionally the same program.**
    - Due to difficulty in testing, the translator has comprehensive error messages when encountering an error.
- Menhir was used to generate the parser/lexer, which allowed flexibility in parsing/lexing, as well as debugging ASTs.
- Typing was crucial, so we made sure to leverage Ocaml's functionality to get rigid types, which can be checked out at `src/lib/syntax_node.ml`
- Some features remain unimplemented due to difficulty & time constraints. Examples:
    - n-dimensional arrays - currently, only one dimensional arrays are supported
    - static variables
    - attributes (__attribtue__, [[likely]], etc.)
    - goto, extern, __asm__, etc.

## Example

Cocaml succesfully compiles the following program (test/loop.c):
```c
int square(int x) {
  return x * x;
}

int main() {
  int x = 0;
  for (int i = 0; i < 10; i = i + 1) {
    x = square(x);
  }
  return x;
}
```

into LLVM IR (viewable at `test/loop.ll`), then into assembly (viewable at `test/loop.s`), and finally into an executable.


## Previous Checkpoints

### Code Checkpoint

For this checkpoint, we focused on building out the main functionality of each piece of a minimally-viable compiler, including the lexer, parser, AST design, and translator. Most of our time was spent doing the up-front work on learning the various tools (i.e. OCamllex, Menhir, and LLVM) and fixing build issues. As of 12/6, we were able to resolve just about all compilation issues within each of the pieces, but unfortunately,  our code checkpoint submission still contains `dune build` that we've yet to figure out, which prevented us from being able to run some of our tests in `test/test.ml`.  

Nonetheless, we feel we are in reasonably good place, having built out the majority of the logic that will go into our final product, as well as the logic to test it. These will no doubt need refining, but we fix the remaining build issues (which, unlike the ones we had fixed, don't seem to do with any logical fallacies of our pieces), we'll be able to focus on debugging, and implementing the remaining behavior.  