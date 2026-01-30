let read_file_lines filename =
  try
    let ic = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec read_all_lines acc =
          match input_line ic with
          | line -> read_all_lines (line :: acc)
          | exception End_of_file -> Array.of_list (List.rev acc)
        in
        read_all_lines [])
  with Sys_error msg ->
    Printf.eprintf "Error: could not read file '%s': %s\n%!" filename msg;
    exit 1

let lex lexbuf lines =
  let rec loop acc =
    match Lexer.token lexbuf with
    | Token.EndOfFile -> List.rev acc
    | tok -> loop (tok :: acc)
  in
  try loop [] with
  | Error.LexerError _ as e ->
      Error.print_error lines e;
      exit 1
  | e ->
      Printf.eprintf "Unexpected lexing error: %s\n%!" (Printexc.to_string e);
      exit 1