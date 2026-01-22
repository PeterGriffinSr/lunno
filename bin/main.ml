open Lunno_library

let lex lexbuf lines =
  let rec aux acc =
    match Lexer.token lexbuf with
    | Token.EndOfFile -> List.rev acc
    | tok -> aux (tok :: acc)
  in
  try aux []
  with Error.LexerError _ as e ->
    Error.print_error lines e;
    exit 1

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <source_file>\n" Sys.argv.(0);
    exit 1);

  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lines =
    let rec read acc =
      try read (input_line ic :: acc) with End_of_file -> List.rev acc
    in
    read []
  in
  let lexbuf = Lexing.from_channel ic in

  let _tokens = lex lexbuf (Array.of_list lines) in
  close_in ic
