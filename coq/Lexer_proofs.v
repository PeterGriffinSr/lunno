From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import Strings.String.
From Stdlib Require Import Lists.List.
From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.

Import ListNotations.
Require Import Token.
Require Import Lexer.

Lemma lex_empty : lex "" = LexOk [TEOF].
Proof. reflexivity. Qed.

Lemma lex_let : lex "let" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_if : lex "if" = LexOk [TIf; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_then : lex "then" = LexOk [TThen; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_else : lex "else" = LexOk [TElse; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_match : lex "match" = LexOk [TMatch; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_import : lex "import" = LexOk [TImport; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_data : lex "data" = LexOk [TData; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_true : lex "true" = LexOk [TTrue; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_false : lex "false" = LexOk [TFalse; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_int_kw : lex "int" = LexOk [TInt; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_float_kw : lex "float" = LexOk [TFloat; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_string_kw : lex "string" = LexOk [TString; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_bool_kw : lex "bool" = LexOk [TBool; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_unit_kw : lex "unit" = LexOk [TUnit; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_list_kw : lex "list" = LexOk [TList; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_arrow : lex "->" = LexOk [TArrow; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_lt : lex "<" = LexOk [TLt; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_gt : lex ">" = LexOk [TGt; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_noteq : lex "<>" = LexOk [TNotEq; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_cons : lex "::" = LexOk [TCons; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_dotdot : lex ".." = LexOk [TDotDot; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_plus : lex "+" = LexOk [TPlus; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_minus : lex "-" = LexOk [TMinus; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_star : lex "*" = LexOk [TStar; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_slash : lex "/" = LexOk [TSlash; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_equals : lex "=" = LexOk [TEquals; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_pipe : lex "|" = LexOk [TPipe; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_colon : lex ":" = LexOk [TColon; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_dot : lex "." = LexOk [TDot; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_comma : lex "," = LexOk [TComma; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_lparen : lex "(" = LexOk [TLParen; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_rparen : lex ")" = LexOk [TRParen; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_lbrack : lex "[" = LexOk [TLBracket; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_rbrack : lex "]" = LexOk [TRBracket; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_lbrace : lex "{" = LexOk [TLBrace; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_rbrace : lex "}" = LexOk [TRBrace; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_wild : lex "_" = LexOk [TWildcard; TEOF]. 
Proof. reflexivity. Qed.

Lemma lex_intlit_zero : lex "0" = LexOk [TIntLit 0; TEOF].
Proof. reflexivity. Qed.

Lemma lex_intlit_one : lex "1" = LexOk [TIntLit 1; TEOF].
Proof. reflexivity. Qed.

Lemma lex_intlit_42 : lex "42" = LexOk [TIntLit 42; TEOF].
Proof. reflexivity. Qed.

Lemma lex_intlit_large : lex "1000000" = LexOk [TIntLit 1000000; TEOF].
Proof. reflexivity. Qed.

Lemma lex_intlit_then_dot : lex "1." = LexOk [TIntLit 1; TDot; TEOF].
Proof. reflexivity. Qed.

Lemma lex_intlit_zero_dot_alpha :
  lex "0.x" = LexOk [TIntLit 0; TDot; TIdent "x"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_two_intlits : lex "1 2" = LexOk [TIntLit 1; TIntLit 2; TEOF].
Proof. reflexivity. Qed.

Lemma lex_int_arith : lex "1 + 2" = LexOk [TIntLit 1; TPlus; TIntLit 2; TEOF].
Proof. reflexivity. Qed.

Lemma lex_float_basic : lex "3.14" = LexOk [TFloatLit "3.14"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_float_zero : lex "0.0" = LexOk [TFloatLit "0.0"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_float_point_five : lex "0.5" = LexOk [TFloatLit "0.5"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_float_long : lex "3.141592" = LexOk [TFloatLit "3.141592"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_float_then_plus : lex "1.5 + 2.0" =
  LexOk [TFloatLit "1.5"; TPlus; TFloatLit "2.0"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_string_empty :
  lex (String """"%char (String """"%char EmptyString)) = LexOk [TStringLit ""; TEOF].
Proof. reflexivity. Qed.

Lemma lex_string_hello :
  lex (String """"%char "hello" ++ (String """"%char EmptyString))
  = LexOk [TStringLit "hello"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_string_with_spaces :
  lex (String """"%char "hi there" ++ (String """"%char EmptyString))
  = LexOk [TStringLit "hi there"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_string_escape_quote :
  lex (String """"%char (String "a"%char (String "\"%char (String """"%char
    (String "b"%char (String """"%char EmptyString))))))
  = LexOk [TStringLit (String "a"%char (String """"%char (String "b"%char EmptyString))); TEOF].
Proof. reflexivity. Qed.

Lemma lex_string_escape_backslash :
  lex (String """"%char (String "a"%char (String "\"%char (String "\"%char
    (String "b"%char (String """"%char EmptyString))))))
  = LexOk [TStringLit (String "a"%char (String "\"%char (String "b"%char EmptyString))); TEOF].
Proof. reflexivity. Qed.

Lemma lex_unterminated_string :
  lex (String """"%char "hello") = LexErr "unterminated string literal".
Proof. reflexivity. Qed.

Lemma lex_ident_simple : lex "foo" = LexOk [TIdent "foo"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_single_char : lex "x" = LexOk [TIdent "x"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_with_digit : lex "foo2" = LexOk [TIdent "foo2"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_with_underscore : lex "foo_bar" = LexOk [TIdent "foo_bar"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_underscore_prefix : lex "_foo" = LexOk [TIdent "_foo"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_double_underscore : lex "__" = LexOk [TIdent "__"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_ident_caps : lex "MyType" = LexOk [TIdent "MyType"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_letx_is_ident : lex "letx" = LexOk [TIdent "letx"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_lets_is_ident : lex "lets" = LexOk [TIdent "lets"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_let2_is_ident : lex "let2" = LexOk [TIdent "let2"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_let_underscore_is_ident : lex "let_" = LexOk [TIdent "let_"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_truefalse_is_ident : lex "truefalse" = LexOk [TIdent "truefalse"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_iffy_is_ident : lex "iffy" = LexOk [TIdent "iffy"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_wildcard_in_match : lex "| _" = LexOk [TPipe; TWildcard; TEOF].
Proof. reflexivity. Qed.

Lemma lex_lt_gt_spaced : lex "< >" = LexOk [TLt; TGt; TEOF].
Proof. reflexivity. Qed.

Lemma lex_colon_not_cons : lex ": :" = LexOk [TColon; TColon; TEOF].
Proof. reflexivity. Qed.

Lemma lex_dot_not_dotdot : lex ". ." = LexOk [TDot; TDot; TEOF].
Proof. reflexivity. Qed.

Lemma lex_minus_then_ident : lex "-x" = LexOk [TMinus; TIdent "x"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_minus_then_int : lex "-1" = LexOk [TMinus; TIntLit 1; TEOF].
Proof. reflexivity. Qed.

Lemma lex_cr_ignored :
  lex (String "013"%char EmptyString) = LexOk [TEOF].
Proof. reflexivity. Qed.

Lemma lex_ws_between_tokens : lex "let	x" = LexOk [TLet; TIdent "x"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_empty_comment : lex "#
let" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_two_comments : lex "# a
# b
let" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_token_then_comment : lex "let # hi" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_comment_mid_code :
  lex "let # hi
x = 1" = LexOk [TLet; TIdent "x"; TEquals; TIntLit 1; TEOF].
Proof. reflexivity. Qed.

Lemma koi_let : keyword_or_ident "let" = TLet.    
Proof. reflexivity. Qed.

Lemma koi_if : keyword_or_ident "if" = TIf.     
Proof. reflexivity. Qed.

Lemma koi_then : keyword_or_ident "then" = TThen.   
Proof. reflexivity. Qed.

Lemma koi_else : keyword_or_ident "else" = TElse.   
Proof. reflexivity. Qed.

Lemma koi_match : keyword_or_ident "match" = TMatch.  
Proof. reflexivity. Qed.

Lemma koi_import : keyword_or_ident "import" = TImport. 
Proof. reflexivity. Qed.

Lemma koi_data : keyword_or_ident "data" = TData.   
Proof. reflexivity. Qed.

Lemma koi_true : keyword_or_ident "true" = TTrue.   
Proof. reflexivity. Qed.

Lemma koi_false : keyword_or_ident "false" = TFalse.  
Proof. reflexivity. Qed.

Lemma koi_int : keyword_or_ident "int" = TInt.    
Proof. reflexivity. Qed.

Lemma koi_float : keyword_or_ident "float" = TFloat.  
Proof. reflexivity. Qed.

Lemma koi_string : keyword_or_ident "string" = TString. 
Proof. reflexivity. Qed.

Lemma koi_bool : keyword_or_ident "bool" = TBool.   
Proof. reflexivity. Qed.

Lemma koi_unit : keyword_or_ident "unit" = TUnit.   
Proof. reflexivity. Qed.

Lemma koi_list : keyword_or_ident "list" = TList.   
Proof. reflexivity. Qed.

Lemma koi_ident : keyword_or_ident "foobar" = TIdent "foobar". 
Proof. reflexivity. Qed.

Lemma koi_empty : keyword_or_ident "" = TIdent "". 
Proof. reflexivity. Qed.

Lemma parse_int_empty : parse_int [] 0%Z = 0%Z.
Proof. reflexivity. Qed.

Lemma parse_int_zero : parse_int ["0"%char] 0%Z = 0%Z.
Proof. reflexivity. Qed.

Lemma parse_int_one : parse_int ["1"%char] 0%Z = 1%Z.
Proof. reflexivity. Qed.

Lemma parse_int_42 : parse_int ["4"%char; "2"%char] 0%Z = 42%Z.
Proof. reflexivity. Qed.

Lemma parse_int_acc : parse_int ["5"%char] 10%Z = 105%Z.
Proof. reflexivity. Qed.

Lemma take_while_all_match : forall (cs : list ascii),
  (forall c, In c cs -> is_digit c = true) ->
  snd (take_while is_digit cs) = [].
Proof.
  induction cs as [| c rest IH].
  - intros _. reflexivity.
  - intros H.
    simpl. rewrite (H c (or_introl eq_refl)).
    destruct (take_while is_digit rest) as [taken remaining] eqn:E.
    simpl. apply IH.
    intros x Hx. apply H. right. exact Hx.
Qed.

Lemma take_while_append : forall p cs taken remaining,
  take_while p cs = (taken, remaining) ->
  cs = taken ++ remaining.
Proof.
  intros p cs. induction cs as [| c rest IH];
  intros taken remaining H.
  - simpl in H. injection H as <- <-. reflexivity.
  - simpl in H. destruct (p c) eqn:Hpc.
    + destruct (take_while p rest) as [t r] eqn:E.
      injection H as <- <-. simpl. f_equal.
      apply (IH t r). reflexivity.
    + injection H as <- <-. reflexivity.
Qed.

Lemma take_while_length : forall p cs taken remaining,
  take_while p cs = (taken, remaining) ->
  length taken + length remaining = length cs.
Proof.
  intros p cs taken remaining H.
  apply take_while_append in H.
  rewrite H. rewrite length_app. lia.
Qed.

Lemma lex_let_binding :
  lex "let x = 42" = LexOk [TLet; TIdent "x"; TEquals; TIntLit 42; TEOF].
Proof. reflexivity. Qed.

Lemma lex_if_expr :
  lex "if true then 1 else 0" =
  LexOk [TIf; TTrue; TThen; TIntLit 1; TElse; TIntLit 0; TEOF].
Proof. reflexivity. Qed.

Lemma lex_cons_list :
  lex "1 :: 2 :: []" =
  LexOk [TIntLit 1; TCons; TIntLit 2; TCons; TLBracket; TRBracket; TEOF].
Proof. reflexivity. Qed.

Lemma lex_match_expr :
  lex "match x | _ -> 1" =
  LexOk [TMatch; TIdent "x"; TPipe; TWildcard; TArrow; TIntLit 1; TEOF].
Proof. reflexivity. Qed.

Lemma lex_type_annotation :
  lex "x : int" = LexOk [TIdent "x"; TColon; TInt; TEOF].
Proof. reflexivity. Qed.

Lemma lex_fn_type :
  lex "int -> bool" = LexOk [TInt; TArrow; TBool; TEOF].
Proof. reflexivity. Qed.

Lemma lex_list_literal :
  lex "[1, 2, 3]" =
  LexOk [TLBracket; TIntLit 1; TComma; TIntLit 2; TComma; TIntLit 3;
         TRBracket; TEOF].
Proof. reflexivity. Qed.

Lemma lex_member_access :
  lex "foo.bar" = LexOk [TIdent "foo"; TDot; TIdent "bar"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_range :
  lex "[0..10]" =
  LexOk [TLBracket; TIntLit 0; TDotDot; TIntLit 10; TRBracket; TEOF].
Proof. reflexivity. Qed.

Lemma lex_not_equal :
  lex "x <> y" = LexOk [TIdent "x"; TNotEq; TIdent "y"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_comparison :
  lex "x < y" = LexOk [TIdent "x"; TLt; TIdent "y"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_import_expr :
  lex "import ""core:io""" = LexOk [TImport; TStringLit "core:io"; TEOF].
Proof. reflexivity. Qed.