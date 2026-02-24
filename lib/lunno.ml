module Debug = Lunno_frontend.Debug
module Flags = Lunno_driver.Flags

let () = Lunno_modules.Std.Init.init ()

let parse_string source =
  let lexbuf = Lexing.from_string source in
  Lunno_frontend.Parser.program Lunno_frontend.Lexer.token lexbuf

let parse_file filename =
  let lines = Lunno_driver.Cli.read_file_lines filename in
  let source = String.concat "\n" (Array.to_list lines) in
  let lexbuf = Lexing.from_string source in
  let program = Lunno_driver.Cli.parse lexbuf lines in
  (lines, program)

let typecheck_program lines program = Lunno_driver.Cli.typecheck program lines
