type pos = { line : int; col : int }
type span = { start : pos; stop : pos }

val dummy_pos : pos
val dummy_span : span
val string_of_pos : pos -> string
val string_of_span : span -> string
val merge : span -> span -> span
