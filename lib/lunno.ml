module Debug = Lunno_debug.Debug
module Flags = Lunno_driver.Flags

let () = Lunno_modules.Core.Init.init ()

let parse_file filename =
  let lines = Lunno_driver.Cli.read_file_lines filename in
  let source = String.concat "\n" (Array.to_list lines) in
  let lexbuf = Lexing.from_string source in
  let program = Lunno_driver.Cli.parse lexbuf in
  (lines, program)

let typecheck_program _lines program = Lunno_driver.Cli.typecheck program
let run_lsp () = Lunno_lsp.Server.run ()
