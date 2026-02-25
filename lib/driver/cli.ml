open Lunno_frontend
open Lunno_lower
open Lunno_common

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

let parse lexbuf lines =
  let module I = Parser.MenhirInterpreter in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let checkpoint = Parser.Incremental.program lexbuf.Lexing.lex_curr_p in
  let rec loop checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
        let checkpoint = I.offer checkpoint (supplier ()) in
        loop checkpoint
    | I.Shifting _ | I.AboutToReduce _ -> loop (I.resume checkpoint)
    | I.Accepted v -> v
    | I.Rejected -> exit 1
    | I.HandlingError env ->
        let pos = lexbuf.Lexing.lex_start_p in
        let state = I.current_state_number env in
        let msg =
          try Parser_errors.message state with Not_found -> "unexpected token"
        in
        let e =
          Error.ParseError
            {
              code = Error.E_Parse_UnexpectedToken;
              msg = String.trim msg;
              span = (pos, pos);
            }
        in
        Error.print_error lines e;
        exit 1
  in
  try loop checkpoint
  with Error.LexerError _ as e ->
    Error.print_error lines e;
    exit 1

let typecheck program lines =
  try Typechecker.infer_program program
  with Error.TypeError _ as e ->
    Error.print_error lines e;
    exit 1
