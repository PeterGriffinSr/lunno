open Lunno

let coq_string_to_string cs = String.init (List.length cs) (List.nth cs)
let ocaml_string_to_coq s = List.init (String.length s) (String.get s)

let show_token tok =
  match tok with
  | Lexer.TLet -> "TLet"
  | Lexer.TIf -> "TIf"
  | Lexer.TThen -> "TThen"
  | Lexer.TElse -> "TElse"
  | Lexer.TMatch -> "TMatch"
  | Lexer.TImport -> "TImport"
  | Lexer.TTrue -> "TTrue"
  | Lexer.TFalse -> "TFalse"
  | Lexer.TInt -> "TInt"
  | Lexer.TFloat -> "TFloat"
  | Lexer.TString -> "TString"
  | Lexer.TBool -> "TBool"
  | Lexer.TUnit -> "TUnit"
  | Lexer.TList -> "TList"
  | Lexer.TIntLit n -> Printf.sprintf "TIntLit(%d)" n
  | Lexer.TFloatLit s -> Printf.sprintf "TFloatLit(%s)" (coq_string_to_string s)
  | Lexer.TStringLit s ->
      Printf.sprintf "TStringLit(%S)" (coq_string_to_string s)
  | Lexer.TIdent s -> Printf.sprintf "TIdent(%s)" (coq_string_to_string s)
  | Lexer.THash -> "THash"
  | Lexer.TLParen -> "TLParen"
  | Lexer.TRParen -> "TRParen"
  | Lexer.TLBracket -> "TLBracket"
  | Lexer.TRBracket -> "TRBracket"
  | Lexer.TLBrace -> "TLBrace"
  | Lexer.TRBrace -> "TRBrace"
  | Lexer.TPlus -> "TPlus"
  | Lexer.TMinus -> "TMinus"
  | Lexer.TStar -> "TStar"
  | Lexer.TSlash -> "TSlash"
  | Lexer.TArrow -> "TArrow"
  | Lexer.TNotEq -> "TNotEq"
  | Lexer.TCons -> "TCons"
  | Lexer.TEquals -> "TEquals"
  | Lexer.TComma -> "TComma"
  | Lexer.TPipe -> "TPipe"
  | Lexer.TColon -> "TColon"
  | Lexer.TWildcard -> "TWildcard"
  | Lexer.TDot -> "TDot"
  | Lexer.TDotDot -> "TDotDot"
  | Lexer.TLt -> "TLt"
  | Lexer.TGt -> "TGt"
  | Lexer.TEOF -> "EOF"

let lex src = Lexer.lex (ocaml_string_to_coq src)
let pass = ref 0
let fail = ref 0

let check name result expected =
  if result = expected then begin
    Printf.printf "PASS: %s\n" name;
    incr pass
  end
  else begin
    Printf.printf "FAIL: %s\n" name;
    Printf.printf "  expected: [%s]\n"
      (String.concat "; " (List.map show_token expected));
    Printf.printf "  got:      [%s]\n"
      (String.concat "; " (List.map show_token result));
    incr fail
  end

let check_lex name src expected =
  match lex src with
  | Lexer.LexErr msg ->
      Printf.printf "FAIL: %s\n  lex error: %s\n" name
        (coq_string_to_string msg);
      incr fail
  | Lexer.LexOk tokens -> check name tokens expected

let check_err name src =
  match lex src with
  | Lexer.LexErr _ ->
      Printf.printf "PASS: %s\n" name;
      incr pass
  | Lexer.LexOk tokens ->
      Printf.printf "FAIL: %s (expected error, got tokens)\n" name;
      Printf.printf "  got: [%s]\n"
        (String.concat "; " (List.map show_token tokens));
      incr fail

let s = ocaml_string_to_coq

let () =
  check_lex "let" "let" [ Lexer.TLet; Lexer.TEOF ];
  check_lex "if" "if" [ Lexer.TIf; Lexer.TEOF ];
  check_lex "then" "then" [ Lexer.TThen; Lexer.TEOF ];
  check_lex "else" "else" [ Lexer.TElse; Lexer.TEOF ];
  check_lex "match" "match" [ Lexer.TMatch; Lexer.TEOF ];
  check_lex "import" "import" [ Lexer.TImport; Lexer.TEOF ];
  check_lex "true" "true" [ Lexer.TTrue; Lexer.TEOF ];
  check_lex "false" "false" [ Lexer.TFalse; Lexer.TEOF ];

  check_lex "int" "int" [ Lexer.TInt; Lexer.TEOF ];
  check_lex "float" "float" [ Lexer.TFloat; Lexer.TEOF ];
  check_lex "string" "string" [ Lexer.TString; Lexer.TEOF ];
  check_lex "bool" "bool" [ Lexer.TBool; Lexer.TEOF ];
  check_lex "unit" "unit" [ Lexer.TUnit; Lexer.TEOF ];
  check_lex "list" "list" [ Lexer.TList; Lexer.TEOF ];

  check_lex "letx is ident" "letx" [ Lexer.TIdent (s "letx"); Lexer.TEOF ];
  check_lex "iffy is ident" "iffy" [ Lexer.TIdent (s "iffy"); Lexer.TEOF ];
  check_lex "truefalse is ident" "truefalse"
    [ Lexer.TIdent (s "truefalse"); Lexer.TEOF ];
  check_lex "let_ is ident" "let_" [ Lexer.TIdent (s "let_"); Lexer.TEOF ];
  check_lex "let2 is ident" "let2" [ Lexer.TIdent (s "let2"); Lexer.TEOF ];
  check_lex "lets is ident" "lets" [ Lexer.TIdent (s "lets"); Lexer.TEOF ];

  check_lex "simple ident" "foo" [ Lexer.TIdent (s "foo"); Lexer.TEOF ];
  check_lex "ident with digit" "foo2" [ Lexer.TIdent (s "foo2"); Lexer.TEOF ];
  check_lex "ident with under" "foo_bar"
    [ Lexer.TIdent (s "foo_bar"); Lexer.TEOF ];
  check_lex "underscore ident" "_foo" [ Lexer.TIdent (s "_foo"); Lexer.TEOF ];
  check_lex "underscore only" "_" [ Lexer.TWildcard; Lexer.TEOF ];
  check_lex "double underscore" "__" [ Lexer.TIdent (s "__"); Lexer.TEOF ];
  check_lex "single char ident" "x" [ Lexer.TIdent (s "x"); Lexer.TEOF ];
  check_lex "caps ident" "MyType" [ Lexer.TIdent (s "MyType"); Lexer.TEOF ];

  check_lex "zero" "0" [ Lexer.TIntLit 0; Lexer.TEOF ];
  check_lex "single digit" "7" [ Lexer.TIntLit 7; Lexer.TEOF ];
  check_lex "multi digit" "42" [ Lexer.TIntLit 42; Lexer.TEOF ];
  check_lex "large int" "1000000" [ Lexer.TIntLit 1000000; Lexer.TEOF ];
  check_lex "int then dot" "1." [ Lexer.TIntLit 1; Lexer.TDot; Lexer.TEOF ];

  check_lex "basic float" "3.14" [ Lexer.TFloatLit (s "3.14"); Lexer.TEOF ];
  check_lex "zero float" "0.0" [ Lexer.TFloatLit (s "0.0"); Lexer.TEOF ];
  check_lex "float no int" "0.5" [ Lexer.TFloatLit (s "0.5"); Lexer.TEOF ];
  check_lex "long float" "3.141592"
    [ Lexer.TFloatLit (s "3.141592"); Lexer.TEOF ];

  check_lex "empty string" {|""|} [ Lexer.TStringLit (s ""); Lexer.TEOF ];
  check_lex "hello string" {|"hello"|}
    [ Lexer.TStringLit (s "hello"); Lexer.TEOF ];
  check_lex "with spaces" {|"hi there"|}
    [ Lexer.TStringLit (s "hi there"); Lexer.TEOF ];
  check_lex "escape newline" {|"a\nb"|}
    [ Lexer.TStringLit (s "a\nb"); Lexer.TEOF ];
  check_lex "escape tab" {|"a\tb"|} [ Lexer.TStringLit (s "a\tb"); Lexer.TEOF ];
  check_lex "escape quote" {|"a\"b"|}
    [ Lexer.TStringLit (s "a\"b"); Lexer.TEOF ];
  check_lex "escape backslash" {|"a\\b"|}
    [ Lexer.TStringLit (s "a\\b"); Lexer.TEOF ];

  check_lex "plus" "+" [ Lexer.TPlus; Lexer.TEOF ];
  check_lex "minus" "-" [ Lexer.TMinus; Lexer.TEOF ];
  check_lex "star" "*" [ Lexer.TStar; Lexer.TEOF ];
  check_lex "slash" "/" [ Lexer.TSlash; Lexer.TEOF ];
  check_lex "equals" "=" [ Lexer.TEquals; Lexer.TEOF ];
  check_lex "pipe" "|" [ Lexer.TPipe; Lexer.TEOF ];
  check_lex "colon" ":" [ Lexer.TColon; Lexer.TEOF ];
  check_lex "dot" "." [ Lexer.TDot; Lexer.TEOF ];
  check_lex "comma" "," [ Lexer.TComma; Lexer.TEOF ];
  check_lex "lparen" "(" [ Lexer.TLParen; Lexer.TEOF ];
  check_lex "rparen" ")" [ Lexer.TRParen; Lexer.TEOF ];
  check_lex "lbracket" "[" [ Lexer.TLBracket; Lexer.TEOF ];
  check_lex "rbracket" "]" [ Lexer.TRBracket; Lexer.TEOF ];
  check_lex "lbrace" "{" [ Lexer.TLBrace; Lexer.TEOF ];
  check_lex "rbrace" "}" [ Lexer.TRBrace; Lexer.TEOF ];
  check_lex "wildcard" "_" [ Lexer.TWildcard; Lexer.TEOF ];

  Printf.printf "\n=== Two-char Symbols ===\n";
  check_lex "arrow" "->" [ Lexer.TArrow; Lexer.TEOF ];
  check_lex "noteq" "<>" [ Lexer.TNotEq; Lexer.TEOF ];
  check_lex "cons" "::" [ Lexer.TCons; Lexer.TEOF ];
  check_lex "dotdot" ".." [ Lexer.TDotDot; Lexer.TEOF ];

  check_lex "lt" "<" [ Lexer.TLt; Lexer.TEOF ];
  check_lex "gt" ">" [ Lexer.TGt; Lexer.TEOF ];
  check_lex "lt gt spaced" "< >" [ Lexer.TLt; Lexer.TGt; Lexer.TEOF ];
  check_lex "colon not cons" ": :" [ Lexer.TColon; Lexer.TColon; Lexer.TEOF ];
  check_lex "dot not dotdot" ". ." [ Lexer.TDot; Lexer.TDot; Lexer.TEOF ];
  check_lex "minus then ident" "-x"
    [ Lexer.TMinus; Lexer.TIdent (s "x"); Lexer.TEOF ];

  check_lex "empty" "" [ Lexer.TEOF ];
  check_lex "spaces only" "   " [ Lexer.TEOF ];
  check_lex "tabs only" "\t\t" [ Lexer.TEOF ];
  check_lex "newlines only" "\n\n" [ Lexer.TEOF ];
  check_lex "mixed ws" " \t\n " [ Lexer.TEOF ];
  check_lex "leading ws" "  let" [ Lexer.TLet; Lexer.TEOF ];
  check_lex "trailing ws" "let  " [ Lexer.TLet; Lexer.TEOF ];
  check_lex "ws between" "let x"
    [ Lexer.TLet; Lexer.TIdent (s "x"); Lexer.TEOF ];
  check_lex "newline between" "let\nx"
    [ Lexer.TLet; Lexer.TIdent (s "x"); Lexer.TEOF ];

  check_lex "comment only" "# hello" [ Lexer.TEOF ];
  check_lex "comment then token" "# hi\nlet" [ Lexer.TLet; Lexer.TEOF ];
  check_lex "token then comment" "let # hi" [ Lexer.TLet; Lexer.TEOF ];
  check_lex "comment mid code" "let # hi\nx = 1"
    [
      Lexer.TLet;
      Lexer.TIdent (s "x");
      Lexer.TEquals;
      Lexer.TIntLit 1;
      Lexer.TEOF;
    ];
  check_lex "two comments" "# a\n# b\nlet" [ Lexer.TLet; Lexer.TEOF ];
  check_lex "empty comment" "#\nlet" [ Lexer.TLet; Lexer.TEOF ];

  check_lex "let binding" "let x = 42"
    [
      Lexer.TLet;
      Lexer.TIdent (s "x");
      Lexer.TEquals;
      Lexer.TIntLit 42;
      Lexer.TEOF;
    ];

  check_lex "let fn" "let f x = x"
    [
      Lexer.TLet;
      Lexer.TIdent (s "f");
      Lexer.TIdent (s "x");
      Lexer.TEquals;
      Lexer.TIdent (s "x");
      Lexer.TEOF;
    ];

  check_lex "if expr" "if true then 1 else 0"
    [
      Lexer.TIf;
      Lexer.TTrue;
      Lexer.TThen;
      Lexer.TIntLit 1;
      Lexer.TElse;
      Lexer.TIntLit 0;
      Lexer.TEOF;
    ];

  check_lex "cons list" "1 :: 2 :: []"
    [
      Lexer.TIntLit 1;
      Lexer.TCons;
      Lexer.TIntLit 2;
      Lexer.TCons;
      Lexer.TLBracket;
      Lexer.TRBracket;
      Lexer.TEOF;
    ];

  check_lex "arithmetic" "1 + 2 * 3"
    [
      Lexer.TIntLit 1;
      Lexer.TPlus;
      Lexer.TIntLit 2;
      Lexer.TStar;
      Lexer.TIntLit 3;
      Lexer.TEOF;
    ];

  check_lex "match expr" "match x | _ -> 1"
    [
      Lexer.TMatch;
      Lexer.TIdent (s "x");
      Lexer.TPipe;
      Lexer.TWildcard;
      Lexer.TArrow;
      Lexer.TIntLit 1;
      Lexer.TEOF;
    ];

  check_lex "type annotation" "x : int"
    [ Lexer.TIdent (s "x"); Lexer.TColon; Lexer.TInt; Lexer.TEOF ];

  check_lex "fn type" "int -> bool"
    [ Lexer.TInt; Lexer.TArrow; Lexer.TBool; Lexer.TEOF ];

  check_lex "tuple" "1, 2, 3"
    [
      Lexer.TIntLit 1;
      Lexer.TComma;
      Lexer.TIntLit 2;
      Lexer.TComma;
      Lexer.TIntLit 3;
      Lexer.TEOF;
    ];

  check_lex "parens" "(1 + 2)"
    [
      Lexer.TLParen;
      Lexer.TIntLit 1;
      Lexer.TPlus;
      Lexer.TIntLit 2;
      Lexer.TRParen;
      Lexer.TEOF;
    ];

  check_lex "not equal" "x <> y"
    [ Lexer.TIdent (s "x"); Lexer.TNotEq; Lexer.TIdent (s "y"); Lexer.TEOF ];

  check_lex "import stmt" "import Foo"
    [ Lexer.TImport; Lexer.TIdent (s "Foo"); Lexer.TEOF ];

  check_err "unterminated string" {|"hello|};
  check_err "unterminated string nl" "\"hello\n";
  check_err "bare backslash" "\\";

  Printf.printf "\n%d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
