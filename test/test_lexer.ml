open OUnit2
open Lunno_frontend
open Lunno_common
open Test_helpers

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
  assert_tokens ~ctxt "+-*/="
    [
      Parser.Plus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Minus (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Asterisk (Lexing.dummy_pos, Lexing.dummy_pos);
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
  assert_tokens ~ctxt "let if then else match data import"
    [
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwIf (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwThen (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwElse (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwMatch (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwData (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.KwImport (Lexing.dummy_pos, Lexing.dummy_pos);
    ]

let test_types ctxt =
  assert_tokens ~ctxt "int i8 i16 i32 i64 float f32 f64 string bool unit"
    [
      Parser.IntegerType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.I8Type (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.I16Type (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.I32Type (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.I64Type (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.FloatingPointType (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.F32Type (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.F64Type (Lexing.dummy_pos, Lexing.dummy_pos);
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
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_multiple_comments ctxt =
  assert_tokens ~ctxt "let #comment1\n #comment2\n x"
    [
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_whitespace ctxt =
  assert_tokens ~ctxt "   \n\t let   x   "
    [
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_whitespace_variants ctxt =
  assert_tokens ~ctxt "let\t x \r\n y"
    [
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
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
    [ Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos) ]

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
      Parser.KwLet (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Equal (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (42L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.KwIf (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.Greater (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.KwThen (Lexing.dummy_pos, Lexing.dummy_pos);
      Parser.Identifier ("x", (Lexing.dummy_pos, Lexing.dummy_pos));
      Parser.KwElse (Lexing.dummy_pos, Lexing.dummy_pos);
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
  | Parser.KwLet (start, stop) ->
      assert_equal ~ctxt 0 start.pos_cnum;
      assert_equal ~ctxt 3 stop.pos_cnum
  | _ -> assert_failure "Expected Let token"

let test_float_zero ctxt =
  assert_tokens ~ctxt "0.0"
    [ Parser.FloatingPoint (0.0, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_leading_zero ctxt =
  assert_tokens ~ctxt "0.5"
    [ Parser.FloatingPoint (0.5, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_exponent_uppercase ctxt =
  assert_tokens ~ctxt "1.0E10"
    [ Parser.FloatingPoint (1.0e10, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_exponent_positive ctxt =
  assert_tokens ~ctxt "1.0e+3"
    [ Parser.FloatingPoint (1.0e3, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_exponent_negative ctxt =
  assert_tokens ~ctxt "1.0e-3"
    [ Parser.FloatingPoint (1.0e-3, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_exponent_zero ctxt =
  assert_tokens ~ctxt "1.0e0"
    [ Parser.FloatingPoint (1.0, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_float_large ctxt =
  assert_tokens ~ctxt "1.7976931348623157e308"
    [
      Parser.FloatingPoint
        (1.7976931348623157e308, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_float_small_exponent ctxt =
  assert_tokens ~ctxt "5.0e-324"
    [ Parser.FloatingPoint (5.0e-324, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_integer_zero ctxt =
  assert_tokens ~ctxt "0"
    [ Parser.Integer (0L, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_integer_one ctxt =
  assert_tokens ~ctxt "1"
    [ Parser.Integer (1L, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_integer_min_int64 ctxt =
  assert_tokens ~ctxt "9223372036854775806"
    [
      Parser.Integer (9223372036854775806L, (Lexing.dummy_pos, Lexing.dummy_pos));
    ]

let test_integer_multi_underscore ctxt =
  assert_tokens ~ctxt "1_000_000"
    [ Parser.Integer (1000000L, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_integer_single_digit_groups ctxt =
  assert_tokens ~ctxt "1_0_0"
    [ Parser.Integer (100L, (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_overflow_large ctxt =
  assert_lexer_error ~ctxt "99999999999999999999" E_Lex_InvalidInt

let test_overflow_by_one ctxt =
  assert_lexer_error ~ctxt "9223372036854775808" E_Lex_InvalidInt

let test_trailing_underscore_after_many_digits ctxt =
  assert_lexer_error ~ctxt "1234_" E_Lex_InvalidInt

let test_multiple_trailing_underscores ctxt =
  assert_lexer_error ~ctxt "1__2" E_Lex_InvalidInt

let test_string_escape_newline ctxt =
  assert_tokens ~ctxt "\"\\n\""
    [ Parser.String ("\n", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_tab ctxt =
  assert_tokens ~ctxt "\"\\t\""
    [ Parser.String ("\t", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_carriage_return ctxt =
  assert_tokens ~ctxt "\"\\r\""
    [ Parser.String ("\r", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_backslash ctxt =
  assert_tokens ~ctxt "\"\\\\\""
    [ Parser.String ("\\", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_double_quote ctxt =
  assert_tokens ~ctxt "\"\\\"\""
    [ Parser.String ("\"", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_at_end ctxt =
  assert_tokens ~ctxt "\"hello\\n\""
    [ Parser.String ("hello\n", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_at_start ctxt =
  assert_tokens ~ctxt "\"\\nworld\""
    [ Parser.String ("\nworld", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_multiple_escapes ctxt =
  assert_tokens ~ctxt "\"\\n\\t\\r\""
    [ Parser.String ("\n\t\r", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_consecutive_backslashes ctxt =
  assert_tokens ~ctxt "\"\\\\\\\\\""
    [ Parser.String ("\\\\", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_escape_quote_in_middle ctxt =
  assert_tokens ~ctxt "\"a\\\"b\""
    [ Parser.String ("a\"b", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_invalid_escape_a ctxt =
  assert_lexer_error ~ctxt "\"\\a\"" E_Lex_InvalidEscape

let test_string_invalid_escape_b ctxt =
  assert_lexer_error ~ctxt "\"\\b\"" E_Lex_InvalidEscape

let test_string_invalid_escape_0 ctxt =
  assert_lexer_error ~ctxt "\"\\0\"" E_Lex_InvalidEscape

let test_string_invalid_escape_u ctxt =
  assert_lexer_error ~ctxt "\"\\u0041\"" E_Lex_InvalidEscape

let test_string_invalid_escape_space ctxt =
  assert_lexer_error ~ctxt "\"\\ \"" E_Lex_InvalidEscape

let test_string_eof_after_escape ctxt =
  assert_lexer_error ~ctxt "\"hello\\" E_Lex_UnterminatedString

let test_string_eof_mid_content ctxt =
  assert_lexer_error ~ctxt "\"abc" E_Lex_UnterminatedString

let test_string_eof_immediately ctxt =
  assert_lexer_error ~ctxt "\"" E_Lex_UnterminatedString

let test_string_newline_after_escape ctxt =
  assert_lexer_error ~ctxt "\"line1\nline2\"" E_Lex_NewlineInString

let test_string_only_spaces ctxt =
  assert_tokens ~ctxt "\" \""
    [ Parser.String (" ", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_string_only_tab ctxt =
  assert_tokens ~ctxt "\"\t\""
    [ Parser.String ("\t", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_unexpected_hash_in_ident ctxt =
  assert_tokens ~ctxt "foo#bar"
    [ Parser.Identifier ("foo", (Lexing.dummy_pos, Lexing.dummy_pos)) ]

let test_unexpected_dollar ctxt =
  assert_lexer_error ~ctxt "$" E_Lex_UnexpectedChar

let test_unexpected_at ctxt =
  assert_lexer_error ~ctxt "@foo" E_Lex_UnexpectedChar

let test_unexpected_backtick ctxt =
  assert_lexer_error ~ctxt "`" E_Lex_UnexpectedChar

let test_unexpected_tilde ctxt =
  assert_lexer_error ~ctxt "~" E_Lex_UnexpectedChar

let test_unexpected_semicolon ctxt =
  assert_lexer_error ~ctxt ";" E_Lex_UnexpectedChar

let test_unexpected_question_mark ctxt =
  assert_lexer_error ~ctxt "?" E_Lex_UnexpectedChar

let test_unexpected_bang ctxt =
  assert_lexer_error ~ctxt "!" E_Lex_UnexpectedChar

let test_unexpected_unicode_cjk ctxt =
  assert_lexer_error ~ctxt "中" E_Lex_UnexpectedChar

let test_unexpected_unicode_arabic ctxt =
  assert_lexer_error ~ctxt "ع" E_Lex_UnexpectedChar

let test_unexpected_unicode_emoji_2 ctxt =
  assert_lexer_error ~ctxt "🚀" E_Lex_UnexpectedChar

let test_unexpected_char_after_valid_tokens ctxt =
  try
    let _ = lex_all "let x @ 1" in
    assert_failure "Expected LexerError, but none was raised"
  with Lexer.LexError e ->
    assert_equal ~ctxt ~printer:Error.string_of_code E_Lex_UnexpectedChar e.code

let suite =
  "lexer tests"
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
         (* numeric: float edge cases *)
         "[lexer] float zero" >:: test_float_zero;
         "[lexer] float leading zero" >:: test_float_leading_zero;
         "[lexer] float exponent uppercase" >:: test_float_exponent_uppercase;
         "[lexer] float exponent positive" >:: test_float_exponent_positive;
         "[lexer] float exponent negative" >:: test_float_exponent_negative;
         "[lexer] float exponent zero" >:: test_float_exponent_zero;
         "[lexer] float large" >:: test_float_large;
         "[lexer] float small exponent" >:: test_float_small_exponent;
         "[lexer] integer zero" >:: test_integer_zero;
         "[lexer] integer one" >:: test_integer_one;
         "[lexer] integer near max int64" >:: test_integer_min_int64;
         "[lexer] integer multi underscore" >:: test_integer_multi_underscore;
         "[lexer] integer single digit groups"
         >:: test_integer_single_digit_groups;
         "[lexer] overflow large" >:: test_overflow_large;
         "[lexer] overflow by one" >:: test_overflow_by_one;
         "[lexer] trailing underscore after many digits"
         >:: test_trailing_underscore_after_many_digits;
         "[lexer] multiple trailing underscores"
         >:: test_multiple_trailing_underscores;
         (* string escapes *)
         "[lexer] string escape newline" >:: test_string_escape_newline;
         "[lexer] string escape tab" >:: test_string_escape_tab;
         "[lexer] string escape carriage return"
         >:: test_string_escape_carriage_return;
         "[lexer] string escape backslash" >:: test_string_escape_backslash;
         "[lexer] string escape double quote"
         >:: test_string_escape_double_quote;
         "[lexer] string escape at end" >:: test_string_escape_at_end;
         "[lexer] string escape at start" >:: test_string_escape_at_start;
         "[lexer] string multiple escapes" >:: test_string_multiple_escapes;
         "[lexer] string consecutive backslashes"
         >:: test_string_consecutive_backslashes;
         "[lexer] string escape quote in middle"
         >:: test_string_escape_quote_in_middle;
         "[lexer] string invalid escape a" >:: test_string_invalid_escape_a;
         "[lexer] string invalid escape b" >:: test_string_invalid_escape_b;
         "[lexer] string invalid escape 0" >:: test_string_invalid_escape_0;
         "[lexer] string invalid escape u" >:: test_string_invalid_escape_u;
         "[lexer] string invalid escape space"
         >:: test_string_invalid_escape_space;
         "[lexer] string eof after escape" >:: test_string_eof_after_escape;
         "[lexer] string eof mid content" >:: test_string_eof_mid_content;
         "[lexer] string eof immediately" >:: test_string_eof_immediately;
         "[lexer] string newline after escape"
         >:: test_string_newline_after_escape;
         "[lexer] string only spaces" >:: test_string_only_spaces;
         "[lexer] string only tab" >:: test_string_only_tab;
         (* error cases *)
         "[lexer] unexpected hash in ident" >:: test_unexpected_hash_in_ident;
         "[lexer] unexpected dollar" >:: test_unexpected_dollar;
         "[lexer] unexpected at" >:: test_unexpected_at;
         "[lexer] unexpected backtick" >:: test_unexpected_backtick;
         "[lexer] unexpected tilde" >:: test_unexpected_tilde;
         "[lexer] unexpected semicolon" >:: test_unexpected_semicolon;
         "[lexer] unexpected question mark" >:: test_unexpected_question_mark;
         "[lexer] unexpected bang" >:: test_unexpected_bang;
         "[lexer] unexpected unicode cjk" >:: test_unexpected_unicode_cjk;
         "[lexer] unexpected unicode arabic" >:: test_unexpected_unicode_arabic;
         "[lexer] unexpected unicode emoji 2"
         >:: test_unexpected_unicode_emoji_2;
         "[lexer] unexpected char after valid tokens"
         >:: test_unexpected_char_after_valid_tokens;
       ]
