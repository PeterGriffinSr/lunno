From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import Strings.String.
From Stdlib Require Import Lists.List.
From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.

Import ListNotations.
Require Import Token.
Require Import Ast.
Require Import Lexer.
Require Import Parser.

Lemma parse_empty : parse [TEOF] = ParseOk empty_program [TEOF].
Proof. vm_compute. reflexivity. Qed.

Lemma parse_int_lit :
  parse [TIntLit 42; TEOF] = ParseOk (mkProgram [] [] [Lit (LInt 42)]) [TEOF].
Proof. vm_compute. reflexivity. Qed.

Lemma parse_true :
  parse [TTrue; TEOF] = ParseOk (mkProgram [] [] [Lit (LBool true)]) [TEOF].
Proof. vm_compute. reflexivity. Qed.