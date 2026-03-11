type pos = { line : int; col : int }
type span = { start : pos; stop : pos }

let dummy_pos = { line = 0; col = 0 }
let dummy_span = { start = dummy_pos; stop = dummy_pos }
let string_of_pos p = Printf.sprintf "%d:%d" p.line p.col

let string_of_span s =
  Printf.sprintf "[%s-%s]" (string_of_pos s.start) (string_of_pos s.stop)

let merge a b = { start = a.start; stop = b.stop }
