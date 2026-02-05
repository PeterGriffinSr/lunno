module Debug = Lunno_frontend.Debug

let parse_string source =
  let lexbuf = Lexing.from_string source in
  Lunno_frontend.Parser.program Lunno_frontend.Lexer.token lexbuf

let parse_file filename =
  let lines = Lunno_driver.Cli.read_file_lines filename in
  let source = String.concat "\n" (Array.to_list lines) in
  let lexbuf = Lexing.from_string source in
  Lunno_driver.Cli.parse lexbuf lines
