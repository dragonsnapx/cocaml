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
      module S = Cocaml.Syntax_node
      module M = Cocaml_main
      
      (* Write a string to a file *)
      let write_file (filename: string) (content: string) =
        Out_channel.with_file filename ~f:(fun out_channel ->
          Out_channel.output_string out_channel content)
    
      (* Convert a Syntax_node.prog to an S-expression string *)
      let prog_to_sexp (prog: S.prog) : string = 
        S.sexp_of_prog prog |> Sexp.to_string_hum

      (* Check if Syntax_node.prog is empty *)
      let is_prog_empty (prog: S.prog) : bool =
        match prog with
          | S.Prog [] -> true
          | _ -> false

      (* Helper function for when we expect a valid AST *)
      let expect_valid_prog (result: M.parse_result) (output_path: string) (test_msg: string) =
        match result with
          | Ok prog ->
              write_file output_path (prog_to_sexp prog);
              assert_bool test_msg (not (is_prog_empty prog))
          | Error msg ->
              Printf.eprintf "Error: %s\n" msg;
              assert_failure "Expected a valid AST, but parsing failed."

      (* Helper function for when we expect an erroneous AST *)
      let expect_parse_error (result: M.parse_result) (expected_msg: string) (test_msg: string) =
        match result with
          | Ok _ ->
            assert_failure "Expected a parsing error, but got a valid AST."
        | Error msg ->
            assert_bool test_msg (String.equal msg expected_msg)
    
      (* Heuristic #1: The number of function definitions should match our expectation *)
      let count_function_definitions (result: M.parse_result) : int =
        match result with
        | Ok (Prog decls) ->
            List.count decls ~f:(function
              | S.Decl.FuncDecl _ -> true
              | _ -> false)
        | Error _ -> 0

      (* Heuristic #2: The number of left and right brackets should be the same *)
      let check_brackets_equal (result: M.parse_result) : bool =
        match result with
        | Ok (Prog decls) ->
            (* Traverse statements to count opening and closing braces *)
            let rec traverse_stmt (open_brace, close_brace) stmt =
              match stmt with
              | S.Stmt.Block (stmts, _) ->
                  List.fold_left stmts ~init:(open_brace + 1, close_brace + 1) ~f:traverse_stmt
              | S.Stmt.If (_, then_stmt, Some else_stmt, _) ->
                  traverse_stmt (traverse_stmt (open_brace, close_brace) then_stmt) else_stmt
              | S.Stmt.If (_, then_stmt, None, _)
              | S.Stmt.While (_, then_stmt, _)
              | S.Stmt.DoWhile (then_stmt, _, _) ->
                  traverse_stmt (open_brace, close_brace) then_stmt
              | S.Stmt.For (_, _, _, body, _) ->
                  traverse_stmt (open_brace, close_brace) body
              | _ -> (open_brace, close_brace)
            in
      
            (* Traverse declarations to count opening and closing braces *)
            let traverse_decl (open_brace, close_brace) decl =
              match decl with
              | S.Decl.FuncDecl (_, _, _, body, _) ->
                  traverse_stmt (open_brace + 1, close_brace + 1) body
              | _ -> (open_brace, close_brace)
            in
      
            (* Count braces across all declarations *)
            let (open_brace, close_brace) = List.fold_left decls ~init:(0, 0) ~f:traverse_decl in
            open_brace = close_brace
        | Error _ -> false
      
      
      let test_parser_simple _ = 
        let input_path = "../../../test/simple.c" in
        let result = M.parse_c_to_ast input_path in
        let output_path = "../../../test/simple_ast.txt" in
        expect_valid_prog result output_path "The simple AST should not be invalid!"

      let test_parser_simple_heuristics _ = 
        let input_path = "../../../test/simple.c" in
        let result = M.parse_c_to_ast input_path in
        assert_equal (count_function_definitions result) 1;
        assert_bool "Brackets should be balanced!" (check_brackets_equal result)

      let test_parser_switch _ = 
        let input_path = "../../../test/switch.c" in
        let result = M.parse_c_to_ast input_path in
        let output_path = "../../../test/switch_ast.txt" in
        expect_valid_prog result output_path "The switch AST should not be invalid!"

      let test_parser_switch_heuristics _ = 
        let input_path = "../../../test/switch.c" in
        let result = M.parse_c_to_ast input_path in
        assert_equal (count_function_definitions result) 1;
        assert_bool "Brackets should be balanced!" (check_brackets_equal result)

      let test_parser_loop _ = 
        let input_path = "../../../test/loop.c" in
        let result = M.parse_c_to_ast input_path in
        let output_path = "../../../test/loop_ast.txt" in
        expect_valid_prog result output_path "The loop AST should not be invalid!"

      let test_parser_loop_heuristics _ = 
        let input_path = "../../../test/loop.c" in
        let result = M.parse_c_to_ast input_path in
        assert_equal (count_function_definitions result) 2;
        assert_bool "Brackets should be balanced!" (check_brackets_equal result)

      let test_parser_loop_undefined_variable _ = 
        let input_path = "../../../test/loop_undefined_variable.c" in
        let result = M.parse_c_to_ast input_path in
        let output_path = "../../../test/loop_undefined_variable_ast.txt" in
        expect_valid_prog result output_path "The loop (with an undefined variable) AST should not be invalid!"

      let test_parser_no_colon _ = 
        let input_path = "../../../test/no_colon.c" in
        let result = M.parse_c_to_ast input_path in
        expect_parse_error result "Syntax error at line 3, column 9" "File missing a semicolon should give an invalid AST!"

      let test_parser_full_c _ = 
        let input_path = "../../../test/large.c" in
        let result = M.parse_c_to_ast input_path in
        let output_path = "../../../test/large_ast.txt" in
        expect_valid_prog result output_path "The large AST should not be invalid!"

      let test_parser_full_c_heuristics _ = 
        let input_path = "../../../test/large.c" in
        let result = M.parse_c_to_ast input_path in
        assert_equal (count_function_definitions result) 3;
        assert_bool "Brackets should be balanced!" (check_brackets_equal result)
        
      let series =
        "Parser Tests" >::: [
          "Parser Base Simple Test" >:: test_parser_simple;
          "Parser Heuristics Simple Test" >:: test_parser_simple_heuristics;
          "Parser Base Switch Test" >:: test_parser_switch;
          "Parser Heuristics Switch Test" >:: test_parser_switch_heuristics;
          "Parser Base Loop Test" >:: test_parser_loop;
          "Parser Heuristics Loop Test" >:: test_parser_loop_heuristics;
          "Parser Base Undefined Loop Test" >:: test_parser_loop_undefined_variable;
          "Parser Base Missing Colon Test" >:: test_parser_no_colon;
          "Parser Base Full C Test" >:: test_parser_full_c;
          "Parser Heuristics Full C Test" >:: test_parser_full_c_heuristics
        ]
    end


module Translator_tests =
  struct
    module X = Cocaml.Syntax_node
    module M = Cocaml.Translator.TranslateFile

    (* Random position for testing *)
    let _pos: X.Position.t = { pos_start = 5; pos_end = 12 }

    let ignore _ = ()

    (* let return_expr: X.stmt = X.Return ((X.IntLiteral (0, _pos)), _pos) *)
    
    let id_x = X.Ident.create("x")
    let id_y = X.Ident.create("y")
    let id_z = X.Ident.create("z")
    let id_p = X.Ident.create("p")
    let id_p_d = X.Ident.create("pd")

    (* int x *)
    let decl_assign_expr = X.Stmt.VarDecl (X.Var_decl ((X.Is_static false), X.VarType.Int, id_x, None, _pos))
    
    (* int y = 3 *)
    let decl_expr = X.Stmt.VarDecl (X.Var_decl ((X.Is_static false), X.VarType.Int, id_y, (Some (X.Expr.IntLiteral (3, _pos))), _pos))

    (* int* p = x *)
    let ptr_decl = X.Stmt.VarDecl(
      X.Var_decl ((X.Is_static false), 
      (X.VarType.Pointer X.VarType.Int), 
      id_p, 
      (Some (X.Expr.PrefixUnOp (X.Expr.Address, X.Expr.Var (id_x, _pos), _pos) )), 
      _pos))

    let ptr_deref = X.Stmt.VarDecl(
      X.Var_decl ((X.Is_static false),
        (X.VarType.Int),
        id_p_d,
        (Some (X.Expr.PrefixUnOp (X.Expr.Dereference, X.Expr.Var (id_p, _pos), _pos) )),
        _pos
      ))

    let bin_op_plus = X.Expr.BinOp (X.Expr.Plus, (X.Expr.IntLiteral (3, _pos)), (X.Expr.IntLiteral (4, _pos)), _pos)
    let decl_calc_int_expr = X.Stmt.VarDecl (X.Var_decl ((X.Is_static false), X.VarType.Int, id_z, Some bin_op_plus, _pos))

    let square_expr = X.Expr.BinOp (X.Expr.Times, (X.Expr.Var (id_x, _pos)), (X.Expr.Var (id_x, _pos)), _pos)
    let make_square = X.Stmt.VarDecl (X.Var_decl ((X.Is_static false), X.VarType.Int, id_y, (Some square_expr), _pos))
    let return_square = X.Stmt.Return ((X.Expr.Var (id_y, _pos)), _pos)
    
    let square_call = X.Expr.Call (X.Ident.create("square"), [X.Expr.IntLiteral (3, _pos)], _pos)

    let msg_struct = X.Ident.create("message")

    let decl_struct = X.Stmt.StructDecl (X.Struct_decl (X.Ident.create("Message"), msg_struct,  Some [ 
      X.Var_decl ((X.Is_static false), (X.VarType.Int), X.Ident.create("status"), None, _pos);
      X.Var_decl ((X.Is_static false), (X.VarType.Long), X.Ident.create("payload"), None, _pos);
    ], _pos))

    let init_struct = X.Stmt.StructInit (X.Struct_init (X.Ident.create("Message"), msg_struct,  None, _pos))
    (* let return_after_call_square_fn_expr: X.stmt = X.Return (square_call, _pos) *)

    (* For simplicity, variable is reused *)

    (* z = msg.status *)
    let access_struct = X.Expr.Assign (X.Expr.Var(id_z, _pos), X.Expr.MemberAccess (X.Expr.Var(msg_struct, _pos), X.Ident.create("status"), _pos), _pos)

    let program : X.prog = X.Prog [
      X.Decl.FuncDecl (
        X.VarType.Int,
        X.Ident.create("square"),
        [
          X.VarType.Int, X.Ident.create("x")
        ],
        X.Stmt.Block ([
          make_square;
          return_square
        ], _pos),
        _pos
      );
      X.Decl.FuncDecl (
        X.VarType.Int,
        X.Ident.create("main"),
        [
          X.VarType.Int, X.Ident.create("argc")
        ],
        X.Stmt.Block ([
          decl_struct;
          init_struct;
          decl_expr;
          decl_assign_expr;
          ptr_decl;
          ptr_deref;
          decl_calc_int_expr;
          X.Stmt.ExprStmt (access_struct, _pos);
          X.Stmt.ExprStmt (square_call, _pos);
          X.Stmt.Return (square_call, _pos);
        ], _pos),
        _pos
      )
    ]
    let first_test _ =
      M.generate_llvm_ir program |> ignore;
      M.print_module_to_file "../../../test2.ll";

      assert_equal 1 1

    let series = 
      "Translator tests" >::: [
        "Example test" >:: first_test
      ]
  end

let series =
  "Tests" >:::
  [ 
    Lexer_tests.series;
    Parser_tests.series;
    Translator_tests.series;
    Exec_tests.series ]

let () = run_test_tt_main series