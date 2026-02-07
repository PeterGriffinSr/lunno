open OUnit2
open Lunno_frontend
open Lunno_common.Error

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
  assert_tokens ~ctxt "let function if then else match"
    [
      Parser.Let (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Function (Lexing.dummy_pos, Lexing.dummy_pos);
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

let test_invalid_integer ctxt =
  assert_lexer_error ~ctxt "999999999999999999999999999" E_Lex_InvalidInt

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

let suite =
  "lunno lexer tests"
  >::: [
         "simple tokens" >:: test_simple_tokens;
         "operators" >:: test_operators;
         "comparisons" >:: test_comparisons;
         "punctuation" >:: test_punctuation;
         "keywords" >:: test_keywords;
         "types" >:: test_types;
         "identifiers" >:: test_identifiers;
         "keyword vs identifier" >:: test_keyword_vs_identifier;
         "integers" >:: test_integers;
         "integer underscores" >:: test_integer_underscores;
         "floats" >:: test_floats;
         "invalid integer" >:: test_invalid_integer;
         "simple string" >:: test_simple_string;
         "complex strings" >:: test_complex_strings;
         "string escapes" >:: test_string_escapes;
         "empty string" >:: test_empty_string;
         "newline in string" >:: test_newline_in_string;
         "unterminated string" >:: test_unterminated_string;
         "invalid escape" >:: test_invalid_escape;
         "comments" >:: test_comments;
         "multiple comments" >:: test_multiple_comments;
         "whitespace" >:: test_whitespace;
         "whitespace variants" >:: test_whitespace_variants;
         "unexpected char" >:: test_unexpected_char;
         "unexpected unicode" >:: test_unexpected_unicode;
         "small program" >:: test_small_program;
         "stress identifiers" >:: test_many_identifiers;
       ]

let () = run_test_tt_main suite
