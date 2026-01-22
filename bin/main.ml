open Lunno_library
open Error

let read_file_lines filename =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let lines = ref [] in
      try
        while true do
          lines := input_line ic :: !lines
        done;
        [||]
      with End_of_file -> Array.of_list (List.rev !lines))

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
  let lines = read_file_lines filename in
  let source = String.concat "\n" (Array.to_list lines) in
  let lexbuf = Lexing.from_string source in
  let _tokens = lex lexbuf lines in
  ()
