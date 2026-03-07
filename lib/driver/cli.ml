open Lunno_frontend
open Lunno_lower
open Lunno_common
open Parser.MenhirInterpreter

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

let parse lexbuf =
  let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let checkpoint = Parser.Incremental.program lexbuf.Lexing.lex_curr_p in
  let rec loop checkpoint =
    match checkpoint with
    | InputNeeded _ ->
        let checkpoint = offer checkpoint (supplier ()) in
        loop checkpoint
    | Shifting _ | AboutToReduce _ -> loop (resume checkpoint)
    | Accepted v -> Ok v
    | Rejected ->
        let pos = lexbuf.Lexing.lex_start_p in
        Error.parse_error Error.E_Parse_UnexpectedToken "rejected" (pos, pos)
    | HandlingError env ->
        let pos = lexbuf.Lexing.lex_start_p in
        let state = current_state_number env in
        let msg =
          try Parser_errors.message state with Not_found -> "unexpected token"
        in
        Error.parse_error Error.E_Parse_UnexpectedToken (String.trim msg)
          (pos, pos)
  in
  match loop checkpoint with
  | Ok _ as ok -> ok
  | Error _ as err -> err
  | exception Lexer.LexError e -> Error e

let typecheck program = Typechecker.infer_program program
