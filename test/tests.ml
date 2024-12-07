open Core
open OUnit2
open Lexer
open Cocaml.Menhir_parser
open Lexing

let unimplemented () = ()

module Lexer_tests = 
  struct 
    (* Helper function to parse an input string and return a list of tokens *)
    let tokens_of_string input =
      let lexbuf = Lexing.from_string input in
      let rec collect_tokens acc =
        match Lexer.token lexbuf with
        | EOF -> List.rev acc
        | tok -> collect_tokens (tok :: acc)
      in
      try collect_tokens [] with
        | LexerError (msg, pos) -> failwith msg
      
    let test_lexer_single_token _ =
      assert_equal [INT] (tokens_of_string "int");
      assert_equal [FLOAT] (tokens_of_string "float");
      assert_equal [CHAR] (tokens_of_string "char");
      assert_equal [LPAREN; RPAREN] (tokens_of_string "()")
      
    let test_lexer_literals _ =
      assert_equal [INT_LITERAL 42] (tokens_of_string "42");
      assert_equal [FLOAT_LITERAL 3.14] (tokens_of_string "3.14");
      assert_equal [CHAR_LITERAL 'a'] (tokens_of_string "'a'")
      
    let test_lexer_keywords _ =
      assert_equal [IF; LPAREN; IDENT "x"; RPAREN; LBRACE; RETURN; INT_LITERAL 1; SEMI; RBRACE]
        (tokens_of_string "if (x) { return 1; }")

    let test_lexer_sequences _ = 
      assert_equal [INT; IDENT "x"; SEMI; RETURN; INT_LITERAL 0; SEMI]
        (tokens_of_string "int x; return 0;");
      assert_equal [IF; LPAREN; IDENT "a"; EQEQ; IDENT "b"; RPAREN; LBRACE; RETURN; INT_LITERAL 1; SEMI; RBRACE]
        (tokens_of_string "if (a == b) { return 1; }")

    let test_lexer_comments _ = 
      (* Single-line comment *)
      assert_equal [INT; IDENT "x"; SEMI]
        (tokens_of_string "int x; // This is a single-line comment");
  
      (* Multi-line comment *)
      assert_equal [INT; IDENT "y"; SEMI]
        (tokens_of_string "int y; /* This is a \n multi-line comment */");
  
      (* Mixed comments and code *)
      assert_equal [FLOAT; IDENT "z"; EQ; FLOAT_LITERAL 3.14; SEMI]
        (tokens_of_string "float z = 3.14; // Assigning a value");
      
    let test_lexer_errors _ =
      assert_raises (Failure "Unexpected character at Line 1, column 1: Unexpected character")
        (fun () -> tokens_of_string "$")

    let series =
      "Lexer Tests" >::: [
        "Single Token Tests" >:: test_lexer_single_token;
        "Literal Tests" >:: test_lexer_literals;
        "Keyword Tests" >:: test_lexer_keywords;
        "Token Sequence Tests" >:: test_lexer_sequences;
        "Comment Tests" >:: test_lexer_comments;
        "Error Tests" >:: test_lexer_errors
      ]
  end

  module Parser_tests = struct
    (* 
     * Helper functions 
     * Note: Some of these will likely be used in other test modules, so we will probably extract them 
     *)

    (* Read a file into a string *)
    let read_file filename =
      In_channel.read_all filename
  
    (* Convert an S-expression string to a Syntax_node.prog *)
    let prog_of_sexp sexp_str =
      let sexp = Sexp.of_string sexp_str in
      Syntax_node.prog_of_sexp sexp
  

    let test_parse_c_to_ast _ =
      let input_code = read_file "simple.c" in
      let derived_ast = Main.parse_c_to_ast input_code in
  
      (* Read in simple_ast.txt as the expected tree *)
      let expected_ast =
        read_file "simple_ast_sexp.txt" |> prog_of_sexp
      in
  
      (* Compare the derived tree to the expected tree *)
      assert_equal
        ~cmp:Syntax_node.equal_prog
        ~printer:Syntax_node.show_prog
        expected_ast
        derived_ast

    let series =
      "Parser Tests" >::: [
        "Test Parse C to AST" >:: test_parse_c_to_ast
      ]
  end

module Translator_tests =
  struct
    module X = Cocaml.Syntax_node
    module M = Cocaml.Translator.TranslateFile
    let _pos: X.position = { pos_start = 5; pos_end = 12 }

    let ignore _ = ()


    let program : X.prog = X.Prog [
      X.FuncDecl (
        X.Int ,
        (X.Ident "main"),
        [
        ],
        X.Block ([
        ], _pos),
        _pos
      )
    ]
    let first_test _ =
      M.generate_llvm_ir program |> ignore;
      M.print_module_to_file "../../../test/test.ll";

      assert_equal 1 1

    let series = 
      "Translator tests" >::: [
        "Example test" >:: first_test
      ]
  end

let series =
  "Tests" >:::
  [ 
    Lexer_tests.series
  ; Parser_tests.series
  ; Translator_tests.series ]

let () = run_test_tt_main series