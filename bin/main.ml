open Lunno

let () =
  match Array.to_list Sys.argv with
  | [ _; filename ] ->
      let lines = Cli.read_file_lines filename in
      let source = String.concat "\n" (Array.to_list lines) in
      let lexbuf = Lexing.from_string source in
      ignore (Cli.lex lexbuf lines)
  | _ ->
      Printf.eprintf "Usage: %s <filename>\n%!" Sys.argv.(0);
      exit 1
