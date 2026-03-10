From Stdlib Require Import Strings.String.
From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import ZArith.ZArith.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Bool.Bool.
Import ListNotations.

Require Import Token.

Definition ascii_to_nat (c : ascii) : nat := nat_of_ascii c.

Definition is_whitespace (c : ascii) : bool :=
  match c with
  | " "%char | "009"%char | "010"%char | "013"%char => true
  | _ => false
  end.

Definition is_digit (c : ascii) : bool :=
  let n := ascii_to_nat c in
  andb (48 <=? n) (n <=? 57).

Definition is_alpha (c : ascii) : bool :=
  let n := ascii_to_nat c in
  orb (andb (65 <=? n) (n <=? 90))
      (andb (97 <=? n) (n <=? 122)).

Definition is_alphanum (c : ascii) : bool :=
  orb (is_alpha c) (is_digit c).

Definition is_ident_char (c : ascii) : bool :=
  orb (is_alphanum c) (Ascii.eqb c "_"%char).

Definition digit_val (c : ascii) : Z :=
  Z.of_nat (ascii_to_nat c) - 48.

Definition keyword_or_ident (s : string) : token :=
  if String.eqb s "let"    then TLet
  else if String.eqb s "if"     then TIf
  else if String.eqb s "then"   then TThen
  else if String.eqb s "else"   then TElse
  else if String.eqb s "match"  then TMatch
  else if String.eqb s "import" then TImport
  else if String.eqb s "true"   then TTrue
  else if String.eqb s "false"  then TFalse
  else if String.eqb s "int"    then TInt
  else if String.eqb s "float"  then TFloat
  else if String.eqb s "string" then TString
  else if String.eqb s "bool"   then TBool
  else if String.eqb s "unit"   then TUnit
  else if String.eqb s "list"   then TList
  else TIdent s.

Definition input := list ascii.

Inductive lex_result : Type :=
  | LexOk  (tokens : list token)
  | LexErr (msg : string).

Fixpoint take_while (p : ascii -> bool) (cs : input) : (list ascii * input) :=
  match cs with
  | []      => ([], [])
  | c :: rest =>
      if p c then
        let (taken, remaining) := take_while p rest in
        (c :: taken, remaining)
      else
        ([], cs)
  end.

Definition list_ascii_to_string (cs : list ascii) : string :=
  fold_right (fun c s => String c s) EmptyString cs.

Fixpoint parse_int (cs : list ascii) (acc : Z) : Z :=
  match cs with
  | []      => acc
  | c :: rest => parse_int rest (acc * 10 + digit_val c)
  end.

Fixpoint lex_string_lit (cs : input) (acc : list ascii) : option (string * input) :=
  match cs with
  | []              => None
  | c :: rest =>
      if Ascii.eqb c """"%char then
        Some (list_ascii_to_string (rev acc), rest)
      else if Ascii.eqb c "\"%char then
        match rest with
        | []        => None
        | e :: rest' =>
            let escaped :=
              if Ascii.eqb e "n"%char  then "010"%char
              else if Ascii.eqb e "t"%char  then "009"%char
              else if Ascii.eqb e "\"%char  then "\"%char
              else if Ascii.eqb e """"%char then """"%char
              else e
            in
            lex_string_lit rest' (escaped :: acc)
        end
      else
        lex_string_lit rest (c :: acc)
  end.

Fixpoint lex_aux (fuel : nat) (cs : input) (acc : list token) : lex_result :=
  match fuel with
  | O    => LexErr "lexer out of fuel"
  | S f  =>
    match cs with
    | [] => LexOk (rev (TEOF :: acc))
    | c :: rest =>
      if is_whitespace c then lex_aux f rest acc
      else if Ascii.eqb c "#"%char then
        let (_, rest') := take_while (fun x => negb (Ascii.eqb x "010"%char)) rest in
        lex_aux f rest' acc
      else if Ascii.eqb c """"%char then
        match lex_string_lit rest [] with
        | None           => LexErr "unterminated string literal"
        | Some (s, rest') => lex_aux f rest' (TStringLit s :: acc)
        end
      else if is_digit c then
        let (digits, rest1) := take_while is_digit cs in
        match rest1 with
        | dot :: rest2 =>
          if Ascii.eqb dot "."%char then
            match rest2 with
            | next :: _ =>
              if is_digit next then
                let (frac, rest3) := take_while is_digit rest2 in
                let s := list_ascii_to_string (digits ++ [dot] ++ frac) in
                lex_aux f rest3 (TFloatLit s :: acc)
              else
                let n := parse_int digits 0 in
                lex_aux f rest1 (TIntLit n :: acc)
            | [] =>
                let n := parse_int digits 0 in
                lex_aux f rest1 (TIntLit n :: acc)
            end
          else
            let n := parse_int digits 0 in
            lex_aux f rest1 (TIntLit n :: acc)
        | [] =>
            let n := parse_int digits 0 in
            lex_aux f [] (TIntLit n :: acc)
        end
      else if is_alpha c then
        let (word, rest') := take_while is_ident_char cs in
        let tok := keyword_or_ident (list_ascii_to_string word) in
        lex_aux f rest' (tok :: acc)
      else if Ascii.eqb c "_"%char then
        match rest with
        | next :: _ =>
          if is_ident_char next then
            let (word, rest') := take_while is_ident_char cs in
            lex_aux f rest' (TIdent (list_ascii_to_string word) :: acc)
          else
            lex_aux f rest (TWildcard :: acc)
        | [] => lex_aux f rest (TWildcard :: acc)
        end
      else if Ascii.eqb c "-"%char then
        match rest with
        | next :: rest' =>
          if Ascii.eqb next ">"%char then lex_aux f rest' (TArrow   :: acc)
          else                             lex_aux f rest  (TMinus   :: acc)
        | [] => lex_aux f rest (TMinus :: acc)
        end
      else if Ascii.eqb c "<"%char then
        match rest with
        | next :: rest' =>
          if Ascii.eqb next ">"%char then lex_aux f rest' (TNotEq :: acc)
          else                             lex_aux f rest  (TLt    :: acc)
        | [] => lex_aux f rest (TLt :: acc)
        end
      else if Ascii.eqb c ">"%char then lex_aux f rest (TGt :: acc)
      else if Ascii.eqb c ":"%char then
        match rest with
        | next :: rest' =>
          if Ascii.eqb next ":"%char then lex_aux f rest' (TCons   :: acc)
          else                             lex_aux f rest  (TColon  :: acc)
        | [] => lex_aux f rest (TColon :: acc)
        end
      else if Ascii.eqb c "."%char then
        match rest with
        | next :: rest' =>
          if Ascii.eqb next "."%char then lex_aux f rest' (TDotDot :: acc)
          else                             lex_aux f rest  (TDot    :: acc)
        | [] => lex_aux f rest (TDot :: acc)
        end
      else if Ascii.eqb c "("%char  then lex_aux f rest (TLParen   :: acc)
      else if Ascii.eqb c ")"%char  then lex_aux f rest (TRParen   :: acc)
      else if Ascii.eqb c "["%char  then lex_aux f rest (TLBracket :: acc)
      else if Ascii.eqb c "]"%char  then lex_aux f rest (TRBracket :: acc)
      else if Ascii.eqb c "{"%char  then lex_aux f rest (TLBrace   :: acc)
      else if Ascii.eqb c "}"%char  then lex_aux f rest (TRBrace   :: acc)
      else if Ascii.eqb c "+"%char  then lex_aux f rest (TPlus     :: acc)
      else if Ascii.eqb c "*"%char  then lex_aux f rest (TStar     :: acc)
      else if Ascii.eqb c "/"%char  then lex_aux f rest (TSlash    :: acc)
      else if Ascii.eqb c "="%char  then lex_aux f rest (TEquals   :: acc)
      else if Ascii.eqb c ","%char  then lex_aux f rest (TComma    :: acc)
      else if Ascii.eqb c "|"%char  then lex_aux f rest (TPipe     :: acc)
      else LexErr ("unexpected character: " ++ String c EmptyString)
    end
  end.

Definition string_to_input (s : string) : input :=
  list_ascii_of_string s.

Definition lex (src : string) : lex_result :=
  lex_aux (String.length src + 1) (string_to_input src) [].

Lemma lex_empty : lex "" = LexOk [TEOF].
Proof. reflexivity. Qed.

Lemma lex_let : lex "let" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_if     : lex "if"     = LexOk [TIf;     TEOF]. Proof. reflexivity. Qed.
Lemma lex_then   : lex "then"   = LexOk [TThen;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_else   : lex "else"   = LexOk [TElse;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_match  : lex "match"  = LexOk [TMatch;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_import : lex "import" = LexOk [TImport; TEOF]. Proof. reflexivity. Qed.
Lemma lex_true   : lex "true"   = LexOk [TTrue;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_false  : lex "false"  = LexOk [TFalse;  TEOF]. Proof. reflexivity. Qed.

Lemma lex_int_kw    : lex "int"    = LexOk [TInt;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_float_kw  : lex "float"  = LexOk [TFloat;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_string_kw : lex "string" = LexOk [TString; TEOF]. Proof. reflexivity. Qed.
Lemma lex_bool_kw   : lex "bool"   = LexOk [TBool;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_unit_kw   : lex "unit"   = LexOk [TUnit;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_list_kw   : lex "list"   = LexOk [TList;   TEOF]. Proof. reflexivity. Qed.

Lemma lex_arrow  : lex "->" = LexOk [TArrow;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_lt     : lex "<"  = LexOk [TLt;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_gt     : lex ">"  = LexOk [TGt;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_noteq  : lex "<>" = LexOk [TNotEq; TEOF]. Proof. reflexivity. Qed.
Lemma lex_cons   : lex "::" = LexOk [TCons;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_dotdot : lex ".." = LexOk [TDotDot; TEOF]. Proof. reflexivity. Qed.

Lemma lex_plus    : lex "+" = LexOk [TPlus;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_minus   : lex "-" = LexOk [TMinus;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_star    : lex "*" = LexOk [TStar;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_slash   : lex "/" = LexOk [TSlash;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_equals  : lex "=" = LexOk [TEquals;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_pipe    : lex "|" = LexOk [TPipe;    TEOF]. Proof. reflexivity. Qed.
Lemma lex_colon   : lex ":" = LexOk [TColon;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_dot     : lex "." = LexOk [TDot;     TEOF]. Proof. reflexivity. Qed.
Lemma lex_comma   : lex "," = LexOk [TComma;   TEOF]. Proof. reflexivity. Qed.
Lemma lex_lparen  : lex "(" = LexOk [TLParen;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_rparen  : lex ")" = LexOk [TRParen;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_lbrack  : lex "[" = LexOk [TLBracket;TEOF]. Proof. reflexivity. Qed.
Lemma lex_rbrack  : lex "]" = LexOk [TRBracket;TEOF]. Proof. reflexivity. Qed.
Lemma lex_lbrace  : lex "{" = LexOk [TLBrace;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_rbrace  : lex "}" = LexOk [TRBrace;  TEOF]. Proof. reflexivity. Qed.
Lemma lex_wild    : lex "_" = LexOk [TWildcard; TEOF]. Proof. reflexivity. Qed.

Lemma lex_whitespace_ignored :
  lex "   " = LexOk [TEOF].
Proof. reflexivity. Qed.

Lemma lex_whitespace_between_tokens :
  lex "let   x" = LexOk [TLet; TIdent "x"; TEOF].
Proof. reflexivity. Qed.

Lemma lex_comment_ignored :
  lex "# this is a comment" = LexOk [TEOF].
Proof. reflexivity. Qed.

Lemma lex_comment_then_token :
  lex "# comment
let" = LexOk [TLet; TEOF].
Proof. reflexivity. Qed.

Lemma lex_unterminated_string :
  lex """hello" = LexErr "unterminated string literal".
Proof. reflexivity. Qed.

Lemma keyword_or_ident_unknown :
  keyword_or_ident "foobar" = TIdent "foobar".
Proof. reflexivity. Qed.

Lemma take_while_nil : forall p,
  take_while p [] = ([], []).
Proof. intros p. reflexivity. Qed.

Lemma take_while_stops : forall p c cs,
  p c = false ->
  take_while p (c :: cs) = ([], c :: cs).
Proof.
  intros p c cs H.
  simpl. rewrite H. reflexivity.
Qed.

Lemma take_while_matches : forall p c cs,
  p c = true ->
  fst (take_while p (c :: cs)) = c :: fst (take_while p cs).
Proof.
  intros p c cs H.
  simpl. rewrite H.
  destruct (take_while p cs). reflexivity.
Qed.