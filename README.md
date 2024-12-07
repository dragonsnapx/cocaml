# Structure


## Preprocessor
Transforms preprocessor directives (`#define`, `#include`, etc.) prior to processing

## Lexer
Parse and builds an AST tree based on `Syntax_node.ml` file

## Translator
Translates AST tree to LLVM IR (example from test file available at test/test_result.ll) and returns any errors or syntactical issues via semantic analysis

## Runner
Runs LLVM IR