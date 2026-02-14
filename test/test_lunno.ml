open OUnit2
open Lunno_frontend
open Lunno_common.Error

let parse_expr s =
  let lexbuf = Lexing.from_string s in
  let prog = Parser.program Lexer.token lexbuf in
  match prog with
  | [ expr ] -> expr
  | _ -> failwith "Expected single expression"

let parse_program s =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

let assert_parses ~ctxt input =
  try
    let _ = parse_expr input in
    ()
  with e ->
    assert_failure
      (Printf.sprintf "Failed to parse: %s\nError: %s" input
         (Printexc.to_string e))

let lex_all s =
  let lexbuf = Lexing.from_string s in
  let rec aux acc =
    match Lexer.token lexbuf with
    | Parser.EndOfFile _ -> List.rev acc
    | tok -> aux (tok :: acc)
  in
  aux []

let lex_exn s =
  let lexbuf = Lexing.from_string s in
  Lexer.token lexbuf

let assert_tokens ~ctxt input expected =
  let actual = lex_all input in
  assert_equal ~ctxt
    ~printer:(fun toks -> String.concat ", " toks)
    (expected |> List.map Token.to_string)
    (actual |> List.map Token.to_string)

let assert_lexer_error ~ctxt input expected_code =
  try
    let _ = lex_exn input in
    assert_failure "Expected LexerError, but none was raised"
  with LexerError e ->
    assert_equal ~ctxt ~printer:string_of_code expected_code e.code

let test_simple_tokens ctxt =
  assert_tokens ~ctxt "(){}[]"
    [
      Parser.LeftParen (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.RightParen (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.LeftBrace (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.RightBrace (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.LeftBracket (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.RightBracket (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_operators ctxt =
  assert_tokens ~ctxt "+-*//="
    [
      Parser.Plus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Minus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Asterisk (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Slash (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Slash (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_comparisons ctxt =
  assert_tokens ~ctxt "<<>>"
    [
      Parser.Less (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.NotEqual (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Greater (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_punctuation ctxt =
  assert_tokens ~ctxt ",:->"
    [
      Parser.Comma (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Colon (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Arrow (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_keywords ctxt =
  assert_tokens ~ctxt "let if then else match"
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.If (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Then (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Else (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Match (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_types ctxt =
  assert_tokens ~ctxt "int float string bool unit"
    [
      Parser.IntegerType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.FloatingPointType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.StringType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.BooleanType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.UnitType (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_identifiers ctxt =
  assert_tokens ~ctxt "foo bar_baz foo'"
    [
      Parser.Identifier ("foo", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Identifier ("bar_baz", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Identifier ("foo'", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_keyword_vs_identifier ctxt =
  assert_tokens ~ctxt "letx functiony"
    [
      Parser.Identifier ("letx", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Identifier ("functiony", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_integers ctxt =
  assert_tokens ~ctxt "0 123 1_000"
    [
      Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Integer (123L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Integer (1000L, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_integer_underscores ctxt =
  assert_tokens ~ctxt "1_2_3 0_0_0"
    [
      Parser.Integer (123L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_floats ctxt =
  assert_tokens ~ctxt "1.0 3.14 2.5e10 1.2E-3"
    [
      Parser.FloatingPoint (1.0, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.FloatingPoint (3.14, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.FloatingPoint (2.5e10, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.FloatingPoint (1.2e-3, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_max_int64 ctxt =
  assert_tokens ~ctxt "9223372036854775807"
    [ Parser.Integer (Int64.max_int, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_overflow_int64 ctxt =
  assert_lexer_error ~ctxt "9223372036854775808" E_Lex_InvalidInt

let test_simple_string ctxt =
  assert_tokens ~ctxt "\"hello\""
    [ Parser.String ("hello", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_complex_strings ctxt =
  assert_tokens ~ctxt "\"hello\\nworld\\t!\\\\\""
    [
      Parser.String ("hello\nworld\t!\\", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_string_escapes ctxt =
  assert_tokens ~ctxt "\"a\\n\\t\\r\\\\\\\"\""
    [ Parser.String ("a\n\t\r\\\"", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_empty_string ctxt = assert_lexer_error ~ctxt "\"\"" E_Lex_EmptyString

let test_newline_in_string ctxt =
  assert_lexer_error ~ctxt "\"hello\nworld\"" E_Lex_NewlineInString

let test_unterminated_string ctxt =
  assert_lexer_error ~ctxt "\"hello" E_Lex_UnterminatedString

let test_invalid_escape ctxt =
  assert_lexer_error ~ctxt "\"\\x\"" E_Lex_InvalidEscape

let test_comments ctxt =
  assert_tokens ~ctxt "let # this is a comment\n x"
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_multiple_comments ctxt =
  assert_tokens ~ctxt "let #comment1\n #comment2\n x"
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_whitespace ctxt =
  assert_tokens ~ctxt "   \n\t let   x   "
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_whitespace_variants ctxt =
  assert_tokens ~ctxt "let\t x \r\n y"
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Identifier ("y", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_unexpected_char ctxt =
  assert_lexer_error ~ctxt "@" E_Lex_UnexpectedChar

let test_unexpected_unicode ctxt =
  assert_lexer_error ~ctxt "🙂" E_Lex_UnexpectedChar

let test_invalid_integer_underscore ctxt =
  assert_tokens ~ctxt "_123"
    [ Parser.Identifier ("_123", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_trailing_integer_underscore ctxt =
  assert_lexer_error ~ctxt "123_" E_Lex_InvalidInt

let test_arrow_vs_minus ctxt =
  assert_tokens ~ctxt "-> - >"
    [
      Parser.Arrow (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Minus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Greater (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_operator_chains ctxt =
  assert_tokens ~ctxt "= =>"
    [
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Greater (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_identifier_leading_underscore ctxt =
  assert_tokens ~ctxt "_foo"
    [ Parser.Identifier ("_foo", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_token_adjacency ctxt =
  assert_tokens ~ctxt "x=1+2"
    [
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (1L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Plus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (2L, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_comment_eof ctxt =
  assert_tokens ~ctxt "let # comment"
    [ Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos) ]

let test_comment_inline ctxt =
  assert_tokens ~ctxt "x # hi\n y"
    [
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Identifier ("y", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_string_single_escape ctxt =
  assert_tokens ~ctxt "\"\\\\\""
    [ Parser.String ("\\", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_eof ctxt =
  assert_lexer_error ~ctxt "\"abc\\" E_Lex_UnterminatedString

let test_single_identifier_eof ctxt =
  assert_tokens ~ctxt "hello"
    [ Parser.Identifier ("hello", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_small_program ctxt =
  let code = "let x = 42 if x > 0 then x else 0" in
  assert_tokens ~ctxt code
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (42L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.If (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Greater (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Then (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Else (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_many_identifiers ctxt =
  let input =
    String.concat " " (List.init 1000 (fun i -> "x" ^ string_of_int i))
  in
  let expected =
    List.init 1000 (fun i ->
        Parser.Identifier
          ("x" ^ string_of_int i, (Lexing.dummy_pos, Lexing.dummy_pos)))
  in
  assert_tokens ~ctxt input expected

let test_span_tracking ctxt =
  let lexbuf = Lexing.from_string "let x" in
  match Lexer.token lexbuf with
  | Parser.Let (start, stop) ->
      assert_equal ~ctxt 0 start.pos_cnum;
      assert_equal ~ctxt 3 stop.pos_cnum
  | _ -> assert_failure "Expected Let token"

let test_integer_literal ctxt =
  match parse_expr "42" with
  | IntLiteral (42L, _) -> ()
  | _ -> assert_failure "Expected integer literal"

let test_float_literal ctxt =
  match parse_expr "3.14" with
  | FloatLiteral (f, _) when abs_float (f -. 3.14) < 0.001 -> ()
  | _ -> assert_failure "Expected float literal"

let test_string_literal ctxt =
  match parse_expr "\"hello\"" with
  | StringLiteral ("hello", _) -> ()
  | _ -> assert_failure "Expected string literal"

let test_variable ctxt =
  match parse_expr "x" with
  | Variable ("x", _) -> ()
  | _ -> assert_failure "Expected variable"

let test_simple_addition ctxt =
  match parse_expr "1 + 2" with
  | Binary
      { op = OpAdd; left = IntLiteral (1L, _); right = IntLiteral (2L, _); _ }
    ->
      ()
  | _ -> assert_failure "Expected addition"

let test_addition_left_associative ctxt =
  match parse_expr "1 + 2 + 3" with
  | Binary
      {
        op = OpAdd;
        left =
          Binary
            {
              op = OpAdd;
              left = IntLiteral (1L, _);
              right = IntLiteral (2L, _);
              _;
            };
        right = IntLiteral (3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected left-associative addition"

let test_multiplication_precedence ctxt =
  match parse_expr "1 + 2 * 3" with
  | Binary
      {
        op = OpAdd;
        left = IntLiteral (1L, _);
        right =
          Binary
            {
              op = OpMul;
              left = IntLiteral (2L, _);
              right = IntLiteral (3L, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected multiplication to bind tighter"

let test_parentheses ctxt =
  match parse_expr "(1 + 2) * 3" with
  | Binary
      {
        op = OpMul;
        left = Binary { op = OpAdd; _ };
        right = IntLiteral (3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected parentheses to override precedence"

let test_comparison ctxt =
  match parse_expr "x < y" with
  | Binary
      { op = OpLess; left = Variable ("x", _); right = Variable ("y", _); _ } ->
      ()
  | _ -> assert_failure "Expected less-than comparison"

let test_comparison_precedence ctxt =
  match parse_expr "x + 1 < y * 2" with
  | Binary
      {
        op = OpLess;
        left = Binary { op = OpAdd; _ };
        right = Binary { op = OpMul; _ };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected comparison to have lower precedence"

let test_simple_let ctxt =
  match parse_expr "let x = 42" with
  | Let { name = "x"; ty = None; body = IntLiteral (42L, _); _ } -> ()
  | _ -> assert_failure "Expected let binding"

let test_typed_let ctxt =
  match parse_expr "let x: int = 42" with
  | Let { name = "x"; ty = Some TyInt; body = IntLiteral (42L, _); _ } -> ()
  | _ -> assert_failure "Expected typed let binding"

let test_single_expr_block ctxt =
  match parse_expr "{ 42 }" with
  | Block ([ IntLiteral (42L, _) ], _) -> ()
  | _ -> assert_failure "Expected single-expression block"

let test_multi_expr_block ctxt =
  match parse_expr "{ let x = 1;\n x + 2 }" with
  | Block ([ Let _; Binary _ ], _) -> ()
  | _ -> assert_failure "Expected multi-expression block"

let test_function_no_params ctxt = assert_parses ~ctxt "let f() { 42 }"

let test_function_untyped_params ctxt =
  match parse_expr "let f(x, y) { x + y }" with
  | Let { name = "f"; body = Lambda { params; ret_ty = None; _ }; _ } -> (
      assert_equal ~ctxt 2 (List.length params);
      match params with
      | [ p1; p2 ] ->
          assert_equal ~ctxt "x" p1.name;
          assert_equal ~ctxt "y" p2.name;
          assert_equal ~ctxt None p1.ty;
          assert_equal ~ctxt None p2.ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function definition"

let test_function_typed_params ctxt =
  match parse_expr "let f(x: int, y: int) -> int { x + y }" with
  | Let { name = "f"; body = Lambda { params; ret_ty = Some Ast.TyInt; _ }; _ }
    -> (
      match params with
      | [ p1; p2 ] ->
          assert_equal ~ctxt (Some Ast.TyInt) p1.ty;
          assert_equal ~ctxt (Some Ast.TyInt) p2.ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected typed function"

let test_function_mixed_params ctxt =
  match parse_expr "let f(x: int, y) -> int { x + y }" with
  | Let { body = Lambda { params; _ }; _ } -> (
      match params with
      | [ p1; p2 ] ->
          assert_equal ~ctxt (Some Ast.TyInt) p1.ty;
          assert_equal ~ctxt None p2.ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function with mixed params"

let test_function_uniform_syntax ctxt =
  match parse_expr "let f(int[x, y]) -> int { x + y }" with
  | Let { body = Lambda { params; _ }; _ } -> (
      match params with
      | [ p1; p2 ] ->
          assert_equal ~ctxt "x" p1.name;
          assert_equal ~ctxt "y" p2.name;
          assert_equal ~ctxt (Some Ast.TyInt) p1.ty;
          assert_equal ~ctxt (Some Ast.TyInt) p2.ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function with uniform syntax"

let test_function_mixed_uniform_syntax ctxt =
  match parse_expr "let f(int[x, y], z: string) -> int { x + y }" with
  | Let { body = Lambda { params; _ }; _ } -> (
      assert_equal ~ctxt 3 (List.length params);
      match params with
      | [ p1; p2; p3 ] ->
          assert_equal ~ctxt (Some Ast.TyInt) p1.ty;
          assert_equal ~ctxt (Some Ast.TyInt) p2.ty;
          assert_equal ~ctxt (Some Ast.TyString) p3.ty
      | _ -> assert_failure "Expected 3 params")
  | _ -> assert_failure "Expected function with mixed uniform syntax"

let test_multiple_top_level_exprs ctxt =
  let prog = parse_program "let x = 1\n;let y = 2;\nx + y" in
  assert_equal ~ctxt 3 (List.length prog)

let test_nested_functions ctxt =
  assert_parses ~ctxt
    "let outer(x: int) { let inner(y: int) { x + y }; inner(10) }"

let test_function_call_in_body ctxt =
  assert_parses ~ctxt "let f(x: int) -> int { let double = x * x;\n x + double }"

let test_empty_block_fails ctxt =
  try
    let _ = parse_expr "{}" in
    assert_failure "Empty block should fail"
  with _ -> ()

let test_deeply_nested_arithmetic ctxt =
  assert_parses ~ctxt "1 + 2 * 3 + 4 * 5 + 6"

let test_all_comparison_ops ctxt =
  assert_parses ~ctxt "x < y";
  assert_parses ~ctxt "x > y";
  assert_parses ~ctxt "x = y";
  assert_parses ~ctxt "x <> y"

let suite =
  "lunno lexer tests"
  >::: [
         "[lexer] simple tokens" >:: test_simple_tokens;
         "[lexer] operators" >:: test_operators;
         "[lexer] comparisons" >:: test_comparisons;
         "[lexer] punctuation" >:: test_punctuation;
         "[lexer] keywords" >:: test_keywords;
         "[lexer] types" >:: test_types;
         "[lexer] identifiers" >:: test_identifiers;
         "[lexer] keyword vs identifier" >:: test_keyword_vs_identifier;
         "[lexer] integers" >:: test_integers;
         "[lexer] integer underscores" >:: test_integer_underscores;
         "[lexer] floats" >:: test_floats;
         "[lexer] max int64" >:: test_max_int64;
         "[lexer] overflow int64" >:: test_overflow_int64;
         "[lexer] simple string" >:: test_simple_string;
         "[lexer] complex strings" >:: test_complex_strings;
         "[lexer] string escapes" >:: test_string_escapes;
         "[lexer] empty string" >:: test_empty_string;
         "[lexer] newline in string" >:: test_newline_in_string;
         "[lexer] unterminated string" >:: test_unterminated_string;
         "[lexer] invalid escape" >:: test_invalid_escape;
         "[lexer] comments" >:: test_comments;
         "[lexer] multiple comments" >:: test_multiple_comments;
         "[lexer] whitespace" >:: test_whitespace;
         "[lexer] whitespace variants" >:: test_whitespace_variants;
         "[lexer] unexpected char" >:: test_unexpected_char;
         "[lexer] unexpected unicode" >:: test_unexpected_unicode;
         "[lexer] small program" >:: test_small_program;
         "[lexer] stress identifiers" >:: test_many_identifiers;
         "[lexer] invalid integer underscore"
         >:: test_invalid_integer_underscore;
         "[lexer] trailing integer underscore"
         >:: test_trailing_integer_underscore;
         "[lexer] arrow vs minus" >:: test_arrow_vs_minus;
         "[lexer] operator chains" >:: test_operator_chains;
         "[lexer] identifier leading underscore"
         >:: test_identifier_leading_underscore;
         "[lexer] token adjacency" >:: test_token_adjacency;
         "[lexer] comment eof" >:: test_comment_eof;
         "[lexer] comment inline" >:: test_comment_inline;
         "[lexer] string single escape" >:: test_string_single_escape;
         "[lexer] string escape eof" >:: test_string_escape_eof;
         "[lexer] single identifier eof" >:: test_single_identifier_eof;
         "[lexer] span tracking" >:: test_span_tracking;
         "[parser] integer literal" >:: test_integer_literal;
         "[parser] float literal" >:: test_float_literal;
         "[parser] string literal" >:: test_string_literal;
         "[parser] variable" >:: test_variable;
         "[parser] simple addition" >:: test_simple_addition;
         "[parser] addition left associative" >:: test_addition_left_associative;
         "[parser] multiplication precedence" >:: test_multiplication_precedence;
         "[parser] parentheses" >:: test_parentheses;
         "[parser] deeply nested arithmetic" >:: test_deeply_nested_arithmetic;
         "[parser] comparison" >:: test_comparison;
         "[parser] comparison precedence" >:: test_comparison_precedence;
         "[parser] all comparison ops" >:: test_all_comparison_ops;
         "[parser] simple let" >:: test_simple_let;
         "[parser] typed let" >:: test_typed_let;
         "[parser] single expr block" >:: test_single_expr_block;
         "[parser] multi expr block" >:: test_multi_expr_block;
         "[parser] empty block fails" >:: test_empty_block_fails;
         "[parser] function no params" >:: test_function_no_params;
         "[parser] function untyped params" >:: test_function_untyped_params;
         "[parser] function typed params" >:: test_function_typed_params;
         "[parser] function mixed params" >:: test_function_mixed_params;
         "[parser] function uniform syntax" >:: test_function_uniform_syntax;
         "[parser] function mixed uniform syntax"
         >:: test_function_mixed_uniform_syntax;
         "[parser] nested functions" >:: test_nested_functions;
         "[parser] function call in body" >:: test_function_call_in_body;
         "[parser] multiple top level exprs" >:: test_multiple_top_level_exprs;
       ]

let () = run_test_tt_main suite
