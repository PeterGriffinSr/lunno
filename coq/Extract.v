Require Import Token.
Require Import Lexer.
Require Import Extraction.

Extraction "lexer.ml" lex lex_result token token_beq token_eq_dec.