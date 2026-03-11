open Lunno_common

val position_tokens :
  string -> Lunno.token list -> (Lunno.token * Span.span) list

val lex : string -> ((Lunno.token * Span.span) list, string) result
