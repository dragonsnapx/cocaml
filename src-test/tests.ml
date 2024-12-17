open Core
open OUnit2
open Lexing

module Lexer_tests = 
  struct 
    module L = Cocaml.Lexer
    open Cocaml.Menhir_parser

    (* Helper function to parse an input string and return a list of tokens *)
    let tokens_of_string input =
      let lexbuf = from_string input in
      let rec collect_tokens acc =
        match L.token lexbuf with
        | EOF -> List.rev acc
        | tok -> collect_tokens (tok :: acc)
      in
      try collect_tokens [] with
        | L.LexerError (msg, _) -> failwith msg
      
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
      assert_equal [IF; LPAREN; IDENT "a"; EQUAL; IDENT "b"; RPAREN; LBRACE; RETURN; INT_LITERAL 1; SEMI; RBRACE]
        (tokens_of_string "if (a == b) { return 1; }")

    let test_lexer_character_errors _ =
      assert_raises (Failure "Line 1, column 1: Unexpected character")
        (fun () -> tokens_of_string "$");

      assert_raises (Failure "Line 1, column 6: Unexpected character")
        (fun () -> tokens_of_string "int y#;");
      
      assert_raises (Failure "Line 2, column 1: Unexpected character")
        (fun () -> tokens_of_string "int x;\n@")


    let test_lexer_comments _ = 
      (* Single-line comments *)
      assert_equal [INT; IDENT "x"; SEMI]
        (tokens_of_string "int x; // This is a single-line comment");

      assert_equal [FLOAT; IDENT "z"; ASSIGN; FLOAT_LITERAL 3.14; SEMI; INT; IDENT "y"; SEMI]
        (tokens_of_string "float z = 3.14; // Assigning a value \n int y;");
  
      (* Multi-line comment *)
      assert_equal [INT; IDENT "y"; SEMI]
        (tokens_of_string "int y; /* This is a \n multi-line comment */");

      (* Nested comments *)
      assert_equal [INT; IDENT "y"; SEMI; IDENT "y"; ASSIGN; INT_LITERAL 5; SEMI]
        (tokens_of_string "int y; /* This is a multi-line comment with a * \n // */ \n y = 5;");

      (* Ensure strings inside a comment aren't read *)
      assert_equal [INT; IDENT "y"; SEMI]
        (tokens_of_string "int y; /* \"This string should be ignored\" */")


    let test_lexer_unclosed_comment_errors _ = 
      assert_raises (Failure "Line 1, column 8: Unclosed comment")
        (fun () -> tokens_of_string "int y; /* This is an unclosed comment\n");

      assert_raises (Failure "Line 2, column 1: Unclosed comment")
        (fun () -> tokens_of_string "float x; \n/* Multi-line comment without closure")


    let test_lexer_strings _ = 
      assert_equal [CHAR; STAR; IDENT "str"; ASSIGN; STRING_LITERAL "Hello, World!"; SEMI]
        (tokens_of_string "char* str = \"Hello, World!\";");
  
      assert_equal [CHAR; STAR; IDENT "str"; ASSIGN; STRING_LITERAL "Line1\nLine2"; SEMI]
        (tokens_of_string "char* str = \"Line1\\nLine2\";");
  
      assert_equal [CHAR; STAR; IDENT "str"; ASSIGN; STRING_LITERAL "This\tTabbed"; SEMI]
        (tokens_of_string "char* str = \"This\\tTabbed\";")


    let test_lexer_unclosed_string_errors _ = 
      assert_raises (Failure "Line 1, column 13: Unclosed string literal")
        (fun () -> tokens_of_string "char* str = \"This is an unclosed string;\n");

      assert_raises (Failure "Line 1, column 13: Unclosed string literal")
        (fun () -> tokens_of_string "char* str = \"Unclosed without newline");
      
      assert_raises (Failure "Line 2, column 17: Unclosed string literal")
        (fun () -> tokens_of_string "char* str;\nchar* another = \"Unclosed;\n")
    

    let series =
      "Lexer Tests" >::: [
        "Single Token Tests" >:: test_lexer_single_token;
        "Literal Tests" >:: test_lexer_literals;
        "Keyword Tests" >:: test_lexer_keywords;
        "Token Sequence Tests" >:: test_lexer_sequences;
        "Character Error Tests" >:: test_lexer_character_errors;
        "Comment Tests" >:: test_lexer_comments;
        "Unclosed Comments Tests" >:: test_lexer_unclosed_comment_errors;
        "String Tests" >:: test_lexer_strings;
        "Unclosed String Tests" >:: test_lexer_unclosed_string_errors
      ]
  end

  module Parser_tests =
    struct
      (* 
      * Helper functions 
      * Note: Some of these will likely be used in other test modules, so we will probably extract them 
      *)

      module S = Cocaml.Syntax_node
      module M = Cocaml_main

      (* Read a file into a string *)
      let read_file filename =
        In_channel.read_all filename
    
      (* Convert an S-expression string to a Syntax_node.prog *)
      let prog_of_sexp sexp_str =
        let sexp = Sexp.of_string sexp_str in
        S.prog_of_sexp sexp
    
      let test_parse_c_to_ast _ =
        let input_file = "../../../test/simple.c" in
        let derived_ast = M.parse_c_to_ast input_file in
    
        (* Read in simple_ast.txt as the expected tree *)
        let result_file = "../../../test/simple_ast_sexp.txt" in
        let expected_ast = result_file |> read_file |> prog_of_sexp in
    
        (* Compare the derived tree to the expected tree *)
        assert_equal
          ~cmp:S.equal_prog
          ~printer:S.show_prog
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

    let return_expr: X.stmt = X.Return ((X.IntLiteral (0, _pos)), _pos)
    let decl_assign_expr: X.stmt = X.LocalVarDecl ((X.Is_static false), X.Int, (X.Ident "x"), None, _pos)
    let decl_expr: X.stmt = X.LocalVarDecl ((X.Is_static false), X.Int, (X.Ident "y"), (Some (X.IntLiteral (3, _pos))), _pos)

    let bin_op_plus: X.expr = X.BinOp (X.Plus, (X.IntLiteral (3, _pos)), (X.IntLiteral (4, _pos)), _pos)
    let decl_calc_int_expr: X.stmt = X.LocalVarDecl ((X.Is_static false), X.Int, (X.Ident "z"), Some bin_op_plus, _pos)

    let square_expr: X.expr = X.BinOp (X.Plus, (X.Var ((X.Ident "x"), _pos)), (X.Var ((X.Ident "x"), _pos)), _pos)
    let make_square: X.stmt = X.LocalVarDecl ((X.Is_static false), X.Int, (X.Ident "y"), (Some square_expr), _pos)
    let return_square: X.stmt = X.Return ((X.Var ((X.Ident "y"), _pos)), _pos)
    
    let square_call: X.expr = X.Call ((X.Ident "square"), [X.IntLiteral (3, _pos)], _pos)
    (* let return_after_call_square_fn_expr: X.stmt = X.Return (square_call, _pos) *)

    let program : X.prog = X.Prog [
      X.FuncDecl (
        X.Int,
        (X.Ident "square"),
        [
          X.Int, X.Ident "x"
        ],
        X.Block ([
          make_square;
          return_square
        ], _pos),
        _pos
      );
      X.FuncDecl (
        X.Int,
        (X.Ident "main"),
        [
          X.Int, X.Ident "argc"
        ],
        X.Block ([
          decl_expr;
          decl_assign_expr;
          decl_calc_int_expr;
          X.ExprStmt (square_call, _pos);
          return_expr;
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