open OUnit2
open Lunno
open Error

let lex_all s =
  let lexbuf = Lexing.from_string s in
  let rec aux acc =
    match Lexer.token lexbuf with
    | Token.EndOfFile -> List.rev acc
    | tok -> aux (tok :: acc)
  in
  aux []

let lex_exn s =
  let lexbuf = Lexing.from_string s in
  Lexer.token lexbuf

let assert_tokens ~ctxt input expected =
  let actual = lex_all input in
  assert_equal ~ctxt
    ~printer:(fun toks ->
      toks |> List.map Token.to_string |> String.concat ", ")
    expected actual

let assert_lexer_error ~ctxt input expected_code =
  try
    let _ = lex_exn input in
    assert_failure "Expected LexerError, but none was raised"
  with LexerError e ->
    assert_equal ~ctxt ~printer:string_of_code expected_code e.code

let test_simple_tokens ctxt =
  assert_tokens ~ctxt "(){}[]"
    [ LeftParen; RightParen; LeftBrace; RightBrace; LeftBracket; RightBracket ]

let test_operators ctxt =
  assert_tokens ~ctxt "+-*//=" [ Plus; Minus; Asterisk; Slash; Slash; Equal ]

let test_comparisons ctxt =
  assert_tokens ~ctxt "<<>>" [ Less; NotEqual; Greater ]

let test_punctuation ctxt = assert_tokens ~ctxt ",:->" [ Comma; Colon; Arrow ]

let test_keywords ctxt =
  assert_tokens ~ctxt "let function if then else match case"
    [ Let; Function; If; Then; Else; Match; Case ]

let test_types ctxt =
  assert_tokens ~ctxt "int float string bool unit"
    [ IntegerType; FloatingPointType; StringType; BooleanType; UnitType ]

let test_identifiers ctxt =
  assert_tokens ~ctxt "foo bar_baz foo'"
    [ Identifier "foo"; Identifier "bar_baz"; Identifier "foo'" ]

let test_keyword_vs_identifier ctxt =
  assert_tokens ~ctxt "letx functiony"
    [ Identifier "letx"; Identifier "functiony" ]

let test_integers ctxt =
  assert_tokens ~ctxt "0 123 1_000" [ Integer 0L; Integer 123L; Integer 1000L ]

let test_integer_underscores ctxt =
  assert_tokens ~ctxt "1_2_3 0_0_0" [ Integer 123L; Integer 0L ]

let test_floats ctxt =
  assert_tokens ~ctxt "1.0 3.14 2.5e10 1.2E-3"
    [
      FloatingPoint 1.0;
      FloatingPoint 3.14;
      FloatingPoint 2.5e10;
      FloatingPoint 1.2e-3;
    ]

let test_invalid_integer ctxt =
  assert_lexer_error ~ctxt "999999999999999999999999999" E_Lex_InvalidInt

let test_simple_string ctxt = assert_tokens ~ctxt "\"hello\"" [ String "hello" ]

let test_complex_strings ctxt =
  assert_tokens ~ctxt "\"hello\\nworld\\t!\\\\\"" [ String "hello\nworld\t!\\" ]

let test_string_escapes ctxt =
  assert_tokens ~ctxt "\"a\\n\\t\\r\\\\\\\"\"" [ String "a\n\t\r\\\"" ]

let test_empty_string ctxt = assert_lexer_error ~ctxt "\"\"" E_Lex_EmptyString

let test_newline_in_string ctxt =
  assert_lexer_error ~ctxt "\"hello\nworld\"" E_Lex_NewlineInString

let test_unterminated_string ctxt =
  assert_lexer_error ~ctxt "\"hello" E_Lex_UnterminatedString

let test_invalid_escape ctxt =
  assert_lexer_error ~ctxt "\"\\x\"" E_Lex_InvalidEscape

let test_comments ctxt =
  assert_tokens ~ctxt "let # this is a comment\n x" [ Let; Identifier "x" ]

let test_multiple_comments ctxt =
  assert_tokens ~ctxt "let #comment1\n #comment2\n x" [ Let; Identifier "x" ]

let test_whitespace ctxt =
  assert_tokens ~ctxt "   \n\t let   x   " [ Let; Identifier "x" ]

let test_whitespace_variants ctxt =
  assert_tokens ~ctxt "let\t x \r\n y" [ Let; Identifier "x"; Identifier "y" ]

let test_unexpected_char ctxt =
  assert_lexer_error ~ctxt "@" E_Lex_UnexpectedChar

let test_unexpected_unicode ctxt =
  assert_lexer_error ~ctxt "🙂" E_Lex_UnexpectedChar

let test_small_program ctxt =
  let code = "let x = 42 in if x > 0 then x else 0" in
  assert_tokens ~ctxt code
    [
      Let;
      Identifier "x";
      Equal;
      Integer 42L;
      In;
      If;
      Identifier "x";
      Greater;
      Integer 0L;
      Then;
      Identifier "x";
      Else;
      Integer 0L;
    ]

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
       ]

let () = run_test_tt_main suite
