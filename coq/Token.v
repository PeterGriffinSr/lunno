From Stdlib Require Import Strings.String.
From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import ZArith.ZArith.
From Stdlib Require Import Lists.List.

Inductive token : Type :=
  | TLet
  | TIf
  | TThen
  | TElse
  | TMatch
  | TImport
  | TTrue
  | TFalse
  | TInt
  | TFloat
  | TString
  | TBool
  | TUnit
  | TList
  | TIntLit (n : Z)
  | TFloatLit (s : string)
  | TStringLit (s : string)
  | TIdent (s : string)
  | THash
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TArrow
  | TNotEq
  | TCons
  | TEquals
  | TComma
  | TPipe
  | TColon
  | TWildcard
  | TDot
  | TDotDot
  | TLt
  | TGt
  | TEOF.

Scheme Equality for token.

Require Import Extraction.
From Stdlib Require ExtrOcamlBasic.
From Stdlib Require ExtrOcamlString.
From Stdlib Require ExtrOcamlNatInt.
From Stdlib Require ExtrOcamlZInt.