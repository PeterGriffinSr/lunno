open Vsharplibrary
open Error

let lex lexbuf lines =
  let rec loop acc =
    try
      match Lexer.token lexbuf with
      | Token.EndOfFile -> List.rev acc
      | tok -> loop (tok :: acc)
    with Error.LexerError _ as e ->
      print_error lines e;
      exit 1
  in
  loop []

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <source_file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let lines =
    let ic = open_in filename in
    let rec read_lines acc =
      try read_lines (input_line ic :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    Array.of_list (read_lines [])
  in

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let _tokens = lex lexbuf lines in
  close_in ic
