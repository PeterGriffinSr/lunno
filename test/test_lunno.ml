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

let test_cons_operator ctxt =
  assert_tokens ~ctxt "::" [ Parser.Cons (Lexing.dummy_pos, Lexing.dummy_pos) ]

let test_underscore_token ctxt =
  assert_tokens ~ctxt "_"
    [ Parser.Underscore (Lexing.dummy_pos, Lexing.dummy_pos) ]

let test_pipe_token ctxt =
  assert_tokens ~ctxt "|" [ Parser.Pipe (Lexing.dummy_pos, Lexing.dummy_pos) ]

let test_mixed_operators ctxt =
  assert_tokens ~ctxt "::|->"
    [
      Parser.Cons (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Pipe (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Arrow (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_boolean_literals ctxt =
  assert_tokens ~ctxt "true false"
    [
      Parser.Boolean (true, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Boolean (false, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_unit_literal ctxt =
  assert_tokens ~ctxt "()"
    [
      Parser.LeftParen (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.RightParen (Lexing.dummy_pos, Lexing.dummy_pos);
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
  | Literal (LInt 42L, _) -> ()
  | _ -> assert_failure "Expected integer literal"

let test_float_literal ctxt =
  match parse_expr "3.14" with
  | Literal (LFloat f, _) when abs_float (f -. 3.14) < 0.001 -> ()
  | _ -> assert_failure "Expected float literal"

let test_string_literal ctxt =
  match parse_expr "\"hello\"" with
  | Literal (LString "hello", _) -> ()
  | _ -> assert_failure "Expected string literal"

let test_variable ctxt =
  match parse_expr "x" with
  | Variable ("x", _) -> ()
  | _ -> assert_failure "Expected variable"

let test_simple_addition ctxt =
  match parse_expr "1 + 2" with
  | Binary
      {
        op = OpAdd;
        left = Literal (LInt 1L, _);
        right = Literal (LInt 2L, _);
        _;
      } ->
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
              left = Literal (LInt 1L, _);
              right = Literal (LInt 2L, _);
              _;
            };
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected left-associative addition"

let test_multiplication_precedence ctxt =
  match parse_expr "1 + 2 * 3" with
  | Binary
      {
        op = OpAdd;
        left = Literal (LInt 1L, _);
        right =
          Binary
            {
              op = OpMul;
              left = Literal (LInt 2L, _);
              right = Literal (LInt 3L, _);
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
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected parentheses to override precedence"

let test_subtraction ctxt =
  match parse_expr "5 - 3" with
  | Binary
      {
        op = OpSub;
        left = Literal (LInt 5L, _);
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected subtraction"

let test_division ctxt =
  match parse_expr "10 / 2" with
  | Binary
      {
        op = OpDiv;
        left = Literal (LInt 10L, _);
        right = Literal (LInt 2L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected division"

let test_mixed_arithmetic ctxt =
  match parse_expr "1 + 2 - 3 * 4 / 5" with
  | Binary { op = OpSub; left = Binary { op = OpAdd; _ }; _ } -> ()
  | _ -> assert_failure "Expected mixed arithmetic with correct precedence"

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
  | Let { name = "x"; ty = None; body = Literal (LInt 42L, _); _ } -> ()
  | _ -> assert_failure "Expected let binding"

let test_typed_let ctxt =
  match parse_expr "let x: int = 42" with
  | Let { name = "x"; ty = Some TyInt; body = Literal (LInt 42L, _); _ } -> ()
  | _ -> assert_failure "Expected typed let binding"

let test_single_expr_block ctxt =
  match parse_expr "{ 42 }" with
  | Block ([ Literal (LInt 42L, _) ], _) -> ()
  | _ -> assert_failure "Expected single-expression block"

let test_multi_expr_block ctxt =
  match parse_expr "{ let x = 1\n x + 2 }" with
  | Block ([ Let _; Binary _ ], _) -> ()
  | _ -> assert_failure "Expected multi-expression block"

let test_function_call_no_args ctxt =
  match parse_expr "foo()" with
  | Apply (Variable ("foo", _), [], _) -> ()
  | _ -> assert_failure "Expected function call with no arguments"

let test_function_call_single_arg ctxt =
  match parse_expr "foo(42)" with
  | Apply (Variable ("foo", _), [ Literal (LInt 42L, _) ], _) -> ()
  | _ -> assert_failure "Expected function call with single argument"

let test_function_call_multiple_args ctxt =
  match parse_expr "foo(1, 2, 3)" with
  | Apply (Variable ("foo", _), [ _; _; _ ], _) -> ()
  | _ -> assert_failure "Expected function call with multiple arguments"

let test_function_call_nested ctxt =
  match parse_expr "foo(bar(baz()))" with
  | Apply (Variable ("foo", _), [ Apply (Variable ("bar", _), _, _) ], _) -> ()
  | _ -> assert_failure "Expected nested function calls"

let test_function_call_with_arithmetic ctxt =
  match parse_expr "foo(x + 1, y * 2)" with
  | Apply
      ( Variable ("foo", _),
        [ Binary { op = OpAdd; _ }; Binary { op = OpMul; _ } ],
        _ ) ->
      ()
  | _ -> assert_failure "Expected function call with arithmetic arguments"

let test_chained_function_calls ctxt =
  match parse_expr "foo()()()" with
  | Apply (Apply (Apply (Variable ("foo", _), [], _), [], _), [], _) -> ()
  | _ -> assert_failure "Expected chained function calls"

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
  let prog = parse_program "let x = 1\nlet y = 2\nx + y" in
  assert_equal ~ctxt 3 (List.length prog)

let test_nested_functions ctxt =
  assert_parses ~ctxt
    "let outer(x: int) { let inner(y: int) { x + y } inner(10) }"

let test_function_call_in_body ctxt =
  assert_parses ~ctxt "let f(x: int) -> int { let double = x * x\n x + double }"

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

let test_block_three_exprs ctxt =
  match parse_expr "{ let x = 1\n let y = 2\n x + y }" with
  | Block ([ Let _; Let _; Binary _ ], _) -> ()
  | _ -> assert_failure "Expected block with three expressions"

let test_nested_blocks ctxt =
  match parse_expr "{ { 1 } }" with
  | Block ([ Block ([ Literal (LInt 1L, _) ], _) ], _) -> ()
  | _ -> assert_failure "Expected nested blocks"

let test_block_with_function_call ctxt =
  match parse_expr "{ foo()\n bar() }" with
  | Block ([ Apply _; Apply _ ], _) -> ()
  | _ -> assert_failure "Expected block with function calls"

let test_let_with_arithmetic_body ctxt =
  match parse_expr "let x = 1 + 2 * 3" with
  | Let { body = Binary { op = OpAdd; _ }; _ } -> ()
  | _ -> assert_failure "Expected let with arithmetic body"

let test_let_with_function_call_body ctxt =
  match parse_expr "let x = foo(42)" with
  | Let { body = Apply _; _ } -> ()
  | _ -> assert_failure "Expected let with function call body"

let test_let_with_block_body ctxt =
  match parse_expr "let x = { let y = 1\n y + 1 }" with
  | Let { body = Block _; _ } -> ()
  | _ -> assert_failure "Expected let with block body"

let test_sequential_lets ctxt =
  let prog = parse_program "let x = 1\nlet y = 2\nlet z = 3" in
  match prog with
  | [ Let _; Let _; Let _ ] -> ()
  | _ -> assert_failure "Expected three sequential lets"

let test_recursive_function_detection ctxt =
  match parse_expr "let fib(n: int) -> int { fib(n - 1) }" with
  | Let { body = Lambda { is_recursive = true; _ }; _ } -> ()
  | _ -> assert_failure "Expected recursive function to be detected"

let test_non_recursive_function ctxt =
  match parse_expr "let id(x: int) -> int { x }" with
  | Let { body = Lambda { is_recursive = false; _ }; _ } -> ()
  | _ -> assert_failure "Expected non-recursive function"

let test_function_with_multiple_return_points ctxt =
  assert_parses ~ctxt "let f(x: int) -> int { if x > 0 then x else 0 }"

let test_function_returning_function ctxt =
  assert_parses ~ctxt
    "let outer(x: int) -> int { let inner(y: int) -> int { x + y }\n inner }"

let test_simple_if_else ctxt =
  match parse_expr "if true then 1 else 0" with
  | If
      {
        cond = Literal (LBool true, _);
        then_ = Literal (LInt 1L, _);
        else_ = Some (Literal (LInt 0L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected simple if-else"

let test_if_with_comparison ctxt =
  match parse_expr "if x > 0 then 1 else 1" with
  | If
      {
        cond = Binary { op = OpGreater; _ };
        then_ = Literal (LInt 1L, _);
        else_ = Some (Literal (LInt 1L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if with comparison"

let test_if_without_else ctxt =
  match parse_expr "if x > 0 then x" with
  | If
      {
        cond = Binary { op = OpGreater; _ };
        then_ = Variable ("x", _);
        else_ = None;
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if without else"

let test_nested_if_in_then ctxt =
  match parse_expr "if x > 0 then if y > 0 then 1 else 2 else 3" with
  | If
      {
        cond = Binary { op = OpGreater; left = Variable ("x", _); _ };
        then_ =
          If
            {
              cond = Binary { op = OpGreater; left = Variable ("y", _); _ };
              then_ = Literal (LInt 1L, _);
              else_ = Some (Literal (LInt 2L, _));
              _;
            };
        else_ = Some (Literal (LInt 3L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested if in then branch"

let test_nested_if_in_else ctxt =
  match parse_expr "if x > 0 then 1 else if y > 0 then 2 else 3" with
  | If
      {
        then_ = Literal (LInt 1L, _);
        else_ =
          Some
            (If
               {
                 then_ = Literal (LInt 2L, _);
                 else_ = Some (Literal (LInt 3L, _));
                 _;
               });
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested if in else branch"

let test_dangling_else_associates_nearest ctxt =
  match parse_expr "if x > 0 then if y > 0 then 1 else 2" with
  | If
      {
        cond = Binary { op = OpGreater; left = Variable ("x", _); _ };
        then_ =
          If
            {
              cond = Binary { op = OpGreater; left = Variable ("y", _); _ };
              then_ = Literal (LInt 1L, _);
              else_ = Some (Literal (LInt 2L, _));
              _;
            };
        else_ = None;
        _;
      } ->
      ()
  | _ -> assert_failure "Expected else to bind to nearest if"

let test_if_with_block_branches ctxt =
  match parse_expr "if x > 0 then { let y = x\n y } else { 0 }" with
  | If
      {
        then_ = Block ([ Let _; Variable _ ], _);
        else_ = Some (Block ([ Literal (LInt 0L, _) ], _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if with block branches"

let test_if_with_block_condition ctxt =
  assert_parses ~ctxt "if { let x = compute()\n x > 0 } then 1 else 0"

let test_if_then_arithmetic ctxt =
  match parse_expr "if x > 0 then x + 1 else x - 1" with
  | If
      {
        then_ = Binary { op = OpAdd; _ };
        else_ = Some (Binary { op = OpSub; _ });
        _;
      } ->
      ()
  | _ -> assert_failure "Expected arithmetic in both branches"

let test_if_then_function_call ctxt =
  match parse_expr "if ready then start() else wait()" with
  | If
      {
        then_ = Apply (Variable ("start", _), [], _);
        else_ = Some (Apply (Variable ("wait", _), [], _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected function calls in branches"

let test_if_with_complex_condition ctxt =
  assert_parses ~ctxt "if x > 0 + y * 2 then 1 else 0"

let test_if_with_multiple_comparisons ctxt =
  assert_parses ~ctxt "if x > 0 then if y < 10 then 1 else 2 else 3"

let test_if_with_equality ctxt =
  match parse_expr "if x = 0 then 1 else 0" with
  | If { cond = Binary { op = OpEqual; _ }; _ } -> ()
  | _ -> assert_failure "Expected equality comparison"

let test_if_with_inequality ctxt =
  match parse_expr "if x <> 0 then 1 else 0" with
  | If { cond = Binary { op = OpNotEqual; _ }; _ } -> ()
  | _ -> assert_failure "Expected inequality comparison"

let test_sequential_ifs ctxt =
  let prog = parse_program "if x > 0 then 1 else 0\nif y > 0 then 2 else 0" in
  match prog with
  | [ If _; If _ ] -> ()
  | _ -> assert_failure "Expected two separate if expressions"

let test_if_after_let ctxt =
  let prog = parse_program "let x = 42\nif x > 0 then x else 0" in
  match prog with
  | [ Let _; If _ ] -> ()
  | _ -> assert_failure "Expected let followed by if"

let test_if_with_parenthesized_condition ctxt =
  assert_parses ~ctxt "if (x > 0) then 1 else 0"

let test_if_with_parenthesized_branches ctxt =
  assert_parses ~ctxt "if x > 0 then (1) else (0)"

let test_deeply_nested_ifs ctxt =
  assert_parses ~ctxt "if a then if b then if c then 1 else 2 else 3 else 4"

let test_if_chain_else_if_pattern ctxt =
  assert_parses ~ctxt "if x < 0 then 1 else if x > 0 then 1 else 0"

let test_if_with_let_in_branches ctxt =
  assert_parses ~ctxt "if x > 0 then let y = x else let z = 0"

let test_if_without_else_in_block ctxt =
  match parse_expr "{ if x > 0 then x }" with
  | Block ([ If { else_ = None; _ } ], _) -> ()
  | _ -> assert_failure "Expected if without else inside block"

let test_if_with_string_branches ctxt =
  match parse_expr "if x > 0 then \"positive\" else \"non-positive\"" with
  | If
      {
        then_ = Literal (LString "positive", _);
        else_ = Some (Literal (LString "non-positive", _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected string literals in branches"

let test_if_with_boolean_literals ctxt =
  match parse_expr "if x then true else false" with
  | If
      {
        cond = Variable ("x", _);
        then_ = Literal (LBool true, _);
        else_ = Some (Literal (LBool false, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected boolean literals in branches"

let test_if_result_in_let ctxt =
  match parse_expr "let result = if x > 0 then 1 else 0" with
  | Let { body = If _; _ } -> ()
  | _ -> assert_failure "Expected if expression as let body"

(* Match Expression Tests *)

let test_match_simple_literal ctxt =
  match parse_expr "match x { | 0 -> 1 | 1 -> 2 }" with
  | Match
      {
        scrutinee = Variable ("x", _);
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              guard = None;
              body = Literal (LInt 1L, _);
              _;
            };
            {
              pattern = PIntLiteral (1L, _);
              guard = None;
              body = Literal (LInt 2L, _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected simple match with literal patterns"

let test_match_wildcard ctxt =
  match parse_expr "match x { | _ -> 0 }" with
  | Match
      {
        cases = [ { pattern = PWildcard _; body = Literal (LInt 0L, _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with wildcard pattern"

let test_match_variable_pattern ctxt =
  match parse_expr "match x { | y -> y }" with
  | Match
      {
        cases =
          [ { pattern = PVariable ("y", _); body = Variable ("y", _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with variable pattern"

let test_match_nil_pattern ctxt =
  match parse_expr "match lst { | [] -> 0 }" with
  | Match
      { cases = [ { pattern = PNil _; body = Literal (LInt 0L, _); _ } ]; _ } ->
      ()
  | _ -> assert_failure "Expected match with nil pattern"

let test_match_cons_pattern ctxt =
  match parse_expr "match lst { | x :: xs -> x }" with
  | Match
      {
        cases =
          [
            {
              pattern = PCons (PVariable ("x", _), PVariable ("xs", _), _);
              body = Variable ("x", _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with cons pattern"

let test_match_nested_cons_pattern ctxt =
  match parse_expr "match lst { | x :: y :: zs -> x }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PCons
                  ( PVariable ("x", _),
                    PCons (PVariable ("y", _), PVariable ("zs", _), _),
                    _ );
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with nested cons pattern"

let test_match_with_guard ctxt =
  match parse_expr "match x { | n if n > 0 -> n }" with
  | Match
      {
        cases =
          [
            {
              pattern = PVariable ("n", _);
              guard = Some (Binary { op = OpGreater; _ });
              body = Variable ("n", _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with guard"

let test_match_multiple_guards ctxt =
  match
    parse_expr "match x { | n if n > 0 -> 1 | m if m < 0 -> 1 | _ -> 0 }"
  with
  | Match
      {
        cases =
          [
            { guard = Some (Binary { op = OpGreater; _ }); _ };
            { guard = Some (Binary { op = OpLess; _ }); _ };
            { pattern = PWildcard _; guard = None; _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with multiple guards"

let test_match_string_literal ctxt =
  match parse_expr "match s { | \"hello\" -> 1 | \"world\" -> 2 }" with
  | Match
      {
        cases =
          [
            { pattern = PStringLiteral ("hello", _); _ };
            { pattern = PStringLiteral ("world", _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with string literal patterns"

let test_match_bool_literal ctxt =
  match parse_expr "match b { | true -> 1 | false -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PBooleanLiteral (true, _); _ };
            { pattern = PBooleanLiteral (false, _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with boolean literal patterns"

let test_match_float_literal ctxt =
  match parse_expr "match f { | 3.14 -> 1 | 2.71 -> 2 }" with
  | Match
      {
        cases =
          [
            { pattern = PFloatLiteral (f1, _); _ };
            { pattern = PFloatLiteral (f2, _); _ };
          ];
        _;
      } ->
      assert_bool "Expected 3.14" (abs_float (f1 -. 3.14) < 0.001);
      assert_bool "Expected 2.71" (abs_float (f2 -. 2.71) < 0.001)
  | _ -> assert_failure "Expected match with float literal patterns"

let test_match_complex_body ctxt =
  match parse_expr "match x {| 0 -> { let y = 10\ny + 5 } }" with
  | Match
      {
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              body = Block ([ Let _; Binary { op = OpAdd; _ } ], _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex body"

let test_match_block_body ctxt =
  match parse_expr "match x { | 0 -> { let y = 10\n y } }" with
  | Match
      {
        cases =
          [
            { pattern = PIntLiteral (0L, _); body = Block ([ Let _; _ ], _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with block body"

let test_match_nested_match ctxt =
  match parse_expr "match x { | 0 -> match y { | 1 -> 2 } }" with
  | Match
      { cases = [ { pattern = PIntLiteral (0L, _); body = Match _; _ } ]; _ } ->
      ()
  | _ -> assert_failure "Expected nested match"

let test_match_in_function ctxt =
  match parse_expr "let f(x: int) -> int { match x { | 0 -> 1 | _ -> x } }" with
  | Let
      {
        body =
          Lambda
            {
              body =
                Block
                  ( [
                      Match
                        {
                          cases =
                            [
                              { pattern = PIntLiteral (0L, _); _ };
                              { pattern = PWildcard _; _ };
                            ];
                          _;
                        };
                    ],
                    _ );
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match in function body"

let test_match_complex_scrutinee ctxt =
  match parse_expr "match x + 1 { | 2 -> 1 }" with
  | Match
      {
        scrutinee = Binary { op = OpAdd; _ };
        cases = [ { pattern = PIntLiteral (2L, _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex scrutinee"

let test_match_function_call_scrutinee ctxt =
  match parse_expr "match f(x) { | 0 -> 1 }" with
  | Match { scrutinee = Apply (Variable ("f", _), [ Variable ("x", _) ], _); _ }
    ->
      ()
  | _ -> assert_failure "Expected match with function call scrutinee"

let test_match_parenthesized_pattern ctxt =
  match parse_expr "match x { | (y) -> y }" with
  | Match
      {
        cases =
          [ { pattern = PVariable ("y", _); body = Variable ("y", _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with parenthesized pattern"

let test_match_cons_with_nil ctxt =
  match parse_expr "match lst { | x :: [] -> x }" with
  | Match
      { cases = [ { pattern = PCons (PVariable ("x", _), PNil _, _); _ } ]; _ }
    ->
      ()
  | _ -> assert_failure "Expected match with cons to nil"

let test_match_multiple_cases ctxt =
  match
    parse_expr "match x { | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | _ -> 0 }"
  with
  | Match { cases; _ } -> assert_equal ~ctxt 5 (List.length cases)
  | _ -> assert_failure "Expected match with 5 cases"

let test_match_guard_with_equality ctxt =
  match parse_expr "match x { | n if n = 0 -> 1 }" with
  | Match { cases = [ { guard = Some (Binary { op = OpEqual; _ }); _ } ]; _ } ->
      ()
  | _ -> assert_failure "Expected match with equality guard"

let test_match_guard_with_complex_expr ctxt =
  match parse_expr "match x { | n if n > 0 + 1 -> n }" with
  | Match
      {
        cases =
          [
            {
              guard =
                Some
                  (Binary
                     { op = OpGreater; right = Binary { op = OpAdd; _ }; _ });
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex guard expression"

let test_match_result_in_let ctxt =
  match parse_expr "let result = match x { | 0 -> 1 | _ -> 0 }" with
  | Let { body = Match _; _ } -> ()
  | _ -> assert_failure "Expected match as let body"

let test_match_as_function_argument ctxt =
  match parse_expr "f(match x { | 0 -> 1 | _ -> 0 })" with
  | Apply (Variable ("f", _), [ Match _ ], _) -> ()
  | _ -> assert_failure "Expected match as function argument"

let test_match_sequential ctxt =
  let prog = parse_program "match x { | 0 -> 1 }\nmatch y { | 0 -> 2 }" in
  match prog with
  | [ Match _; Match _ ] -> ()
  | _ -> assert_failure "Expected two sequential matches"

let test_match_after_let ctxt =
  let prog = parse_program "let x = 42\nmatch x { | 42 -> 1 }" in
  match prog with
  | [ Let _; Match _ ] -> ()
  | _ -> assert_failure "Expected let followed by match"

let test_match_cons_right_associative ctxt =
  match parse_expr "match lst { | a :: b :: c :: [] -> a }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PCons
                  ( PVariable ("a", _),
                    PCons
                      ( PVariable ("b", _),
                        PCons (PVariable ("c", _), PNil _, _),
                        _ ),
                    _ );
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected right-associative cons pattern"

let test_match_guard_with_function_call ctxt =
  match parse_expr "match x { | n if is_positive(n) -> n }" with
  | Match
      {
        cases =
          [ { guard = Some (Apply (Variable ("is_positive", _), _, _)); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with function call in guard"

let test_match_literal_then_wildcard ctxt =
  match parse_expr "match x { | 0 -> 1 | 1 -> 2 | _ -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PIntLiteral (0L, _); _ };
            { pattern = PIntLiteral (1L, _); _ };
            { pattern = PWildcard _; _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected literals followed by wildcard"

let test_match_fibonacci_style ctxt =
  match
    parse_expr "match n { | 0 -> 0 | 1 -> 1 | m -> fib(m - 1) + fib(m - 2) }"
  with
  | Match
      {
        cases =
          [
            { pattern = PIntLiteral (0L, _); body = Literal (LInt 0L, _); _ };
            { pattern = PIntLiteral (1L, _); body = Literal (LInt 1L, _); _ };
            { pattern = PVariable ("m", _); body = Binary { op = OpAdd; _ }; _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected fibonacci-style match"

let test_match_list_length_style ctxt =
  match parse_expr "match lst { | [] -> 0 | _ :: xs -> 1 + length(xs) }" with
  | Match
      {
        cases =
          [
            { pattern = PNil _; body = Literal (LInt 0L, _); _ };
            {
              pattern = PCons (PWildcard _, PVariable ("xs", _), _);
              body = Binary { op = OpAdd; _ };
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected list length style match"

let test_match_with_if_in_body ctxt =
  match parse_expr "match x { | n -> if n > 0 then n else 0 }" with
  | Match { cases = [ { pattern = PVariable ("n", _); body = If _; _ } ]; _ } ->
      ()
  | _ -> assert_failure "Expected match with if in body"

let test_match_exhaustive_bool ctxt =
  match parse_expr "match b { | true -> 1 | false -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PBooleanLiteral (true, _); _ };
            { pattern = PBooleanLiteral (false, _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected exhaustive boolean match"

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
         "[lexer] cons operator" >:: test_cons_operator;
         "[lexer] underscore token" >:: test_underscore_token;
         "[lexer] pipe token" >:: test_pipe_token;
         "[lexer] mixed operators" >:: test_mixed_operators;
         "[lexer] boolean literals" >:: test_boolean_literals;
         "[lexer] unit literal" >:: test_unit_literal;
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
         "[parser] simple if else" >:: test_simple_if_else;
         "[parser] if with comparison" >:: test_if_with_comparison;
         "[parser] if without else" >:: test_if_without_else;
         "[parser] nested if in then" >:: test_nested_if_in_then;
         "[parser] nested if in else" >:: test_nested_if_in_else;
         "[parser] dangling else associates nearest"
         >:: test_dangling_else_associates_nearest;
         "[parser] if with block branches" >:: test_if_with_block_branches;
         "[parser] if with block condition" >:: test_if_with_block_condition;
         "[parser] if then arithmetic" >:: test_if_then_arithmetic;
         "[parser] if then function call" >:: test_if_then_function_call;
         "[parser] if with complex condition" >:: test_if_with_complex_condition;
         "[parser] if with multiple comparisons"
         >:: test_if_with_multiple_comparisons;
         "[parser] if with equality" >:: test_if_with_equality;
         "[parser] if with inequality" >:: test_if_with_inequality;
         "[parser] sequential ifs" >:: test_sequential_ifs;
         "[parser] if after let" >:: test_if_after_let;
         "[parser] if with parenthesized condition"
         >:: test_if_with_parenthesized_condition;
         "[parser] if with parenthesized branches"
         >:: test_if_with_parenthesized_branches;
         "[parser] deeply nested ifs" >:: test_deeply_nested_ifs;
         "[parser] if chain else if pattern" >:: test_if_chain_else_if_pattern;
         "[parser] if with let in branches" >:: test_if_with_let_in_branches;
         "[parser] if without else in block" >:: test_if_without_else_in_block;
         "[parser] if with string branches" >:: test_if_with_string_branches;
         "[parser] if with boolean literals" >:: test_if_with_boolean_literals;
         "[parser] if result in let" >:: test_if_result_in_let;
         "[parser] match simple literal" >:: test_match_simple_literal;
         "[parser] match wildcard" >:: test_match_wildcard;
         "[parser] match variable pattern" >:: test_match_variable_pattern;
         "[parser] match nil pattern" >:: test_match_nil_pattern;
         "[parser] match cons pattern" >:: test_match_cons_pattern;
         "[parser] match nested cons pattern" >:: test_match_nested_cons_pattern;
         "[parser] match with guard" >:: test_match_with_guard;
         "[parser] match multiple guards" >:: test_match_multiple_guards;
         "[parser] match string literal" >:: test_match_string_literal;
         "[parser] match bool literal" >:: test_match_bool_literal;
         "[parser] match float literal" >:: test_match_float_literal;
         "[parser] match complex body" >:: test_match_complex_body;
         "[parser] match block body" >:: test_match_block_body;
         "[parser] match nested match" >:: test_match_nested_match;
         "[parser] match in function" >:: test_match_in_function;
         "[parser] match complex scrutinee" >:: test_match_complex_scrutinee;
         "[parser] match function call scrutinee"
         >:: test_match_function_call_scrutinee;
         "[parser] match parenthesized pattern"
         >:: test_match_parenthesized_pattern;
         "[parser] match cons with nil" >:: test_match_cons_with_nil;
         "[parser] match multiple cases" >:: test_match_multiple_cases;
         "[parser] match guard with equality" >:: test_match_guard_with_equality;
         "[parser] match guard with complex expr"
         >:: test_match_guard_with_complex_expr;
         "[parser] match result in let" >:: test_match_result_in_let;
         "[parser] match as function argument"
         >:: test_match_as_function_argument;
         "[parser] match sequential" >:: test_match_sequential;
         "[parser] match after let" >:: test_match_after_let;
         "[parser] match cons right associative"
         >:: test_match_cons_right_associative;
         "[parser] match guard with function call"
         >:: test_match_guard_with_function_call;
         "[parser] match literal then wildcard"
         >:: test_match_literal_then_wildcard;
         "[parser] match fibonacci style" >:: test_match_fibonacci_style;
         "[parser] match list length style" >:: test_match_list_length_style;
         "[parser] match with if in body" >:: test_match_with_if_in_body;
         "[parser] match exhaustive bool" >:: test_match_exhaustive_bool;
         "[parser] subtraction" >:: test_subtraction;
         "[parser] division" >:: test_division;
         "[parser] mixed arithmetic" >:: test_mixed_arithmetic;
         "[parser] function call no args" >:: test_function_call_no_args;
         "[parser] function call single arg" >:: test_function_call_single_arg;
         "[parser] function call multiple args"
         >:: test_function_call_multiple_args;
         "[parser] function call nested" >:: test_function_call_nested;
         "[parser] function call with arithmetic"
         >:: test_function_call_with_arithmetic;
         "[parser] chained function calls" >:: test_chained_function_calls;
         "[parser] block three exprs" >:: test_block_three_exprs;
         "[parser] nested blocks" >:: test_nested_blocks;
         "[parser] block with function call" >:: test_block_with_function_call;
         "[parser] let with arithmetic body" >:: test_let_with_arithmetic_body;
         "[parser] let with function call body"
         >:: test_let_with_function_call_body;
         "[parser] let with block body" >:: test_let_with_block_body;
         "[parser] sequential lets" >:: test_sequential_lets;
         "[parser] recursive function detection"
         >:: test_recursive_function_detection;
         "[parser] non recursive function" >:: test_non_recursive_function;
         "[parser] function with multiple return points"
         >:: test_function_with_multiple_return_points;
         "[parser] function returning function"
         >:: test_function_returning_function;
       ]

let () = run_test_tt_main suite
