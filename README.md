# Code Checkpoint

For this checkpoint, we focused on building out the main functionality of each piece of a minimally-viable compiler, including the lexer, parser, AST design, and translator. Most of our time was spent doing the up-front work on learning the various tools (i.e. OCamllex, Menhir, and LLVM) and fixing build issues. As of 12/6, we were able to resolve just about all compilation issues within each of the pieces, but unfortunately,  our code checkpoint submission still contains `dune build` that we've yet to figure out, which prevented us from being able to run some of our tests in `test/test.ml`.  

Nonetheless, we feel we are in reasonably good place, having built out the majority of the logic that will go into our final product, as well as the logic to test it. These will no doubt need refining, but we fix the remaining build issues (which, unlike the ones we had fixed, don't seem to do with any logical fallacies of our pieces), we'll be able to focus on debugging, and implementing the remaining behavior.  


## Structure

### Preprocessor
Transforms preprocessor directives (`#define`, `#include`, etc.) prior to processing. This is a "time-permitting" goal of our implementation, and we did not work on this during the checkpoint.

### Main
Main interface from our compiler, which will take a .c file and has endpoints to return one of an AST, LLVM IR, or an executable. Currently, we've only implemented the `parse_c_to_ast` endpoint, which is used in our tests for our set of Parser tests. Once our parser's behavior is better verified, we will add our other endpoints (note that already implemented a `generate_llvm_ir` function in the translator to help test it independently, which the eventual `generate_llvm` endpoint in Main will closely resemble)


### Lexer
Given a set of "tokens" from `Menhir_parser`, the Lexer performs regex matching to extract these tokens from our given .c file (which is passed to the Lexer as a `Lexing.lexbuf` data structure). This compiles successfully, which can be seen with the `ocamllex lexer.mll` command (dune runs this command automatically during our compilation process). 

### Parser
Given a stream of tokens passed from the Lexer, the Parser checks this stream against a set of "grammar rules", and uses these to build an AST, according to the `Syntax_node` interface. This is the end of the front-end of the compiler -- the back-end simply receives an AST, which it interacts with via `Syntax_node`. At the top-level, our AST is a list of "declarations"

### Translator
Translates AST tree to LLVM IR (example from test file available at test/test_result.ll) and returns any errors or syntactical issues via semantic analysis. The task of semantic analysis is made easier by directly using the OCaml `llvm` library, which allows us to interact with the translated code directly. 

### Runner
Runs LLVM IR, generating an executable. This step will use LLVM itself, and has not been something we've worked on yet. 
