open Core
open OUnit2
open Lexer
open Parser
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
      unimplemented ()

    let test_lexer_comments _ = 
      unimplemented ()
      
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

let series =
  "Tests" >:::
  [ Lexer_tests.series]

let () = run_test_tt_main series