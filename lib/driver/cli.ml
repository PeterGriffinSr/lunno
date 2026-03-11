open Lunno_frontend
open Lunno_common

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let src = Bytes.create n in
  really_input ic src 0 n;
  close_in ic;
  Bytes.to_string src

let parse_file path =
  let src = read_file path in
  let tokens =
    match Lunno.lex (Util.ocaml_string_to_coq src) with
    | Lunno.LexErr msg ->
        Printf.eprintf "Lex error: %s\n" (Util.coq_string_to_string msg);
        exit 1
    | Lunno.LexOk toks -> toks
  in
  let prog =
    match Lunno.parse tokens with
    | Lunno.ParseErr msg ->
        Printf.eprintf "Parse error: %s\n" (Util.coq_string_to_string msg);
        exit 1
    | Lunno.ParseOk (p, _) -> p
  in
  let positioned = Positioned_lexer.position_tokens src tokens in
  Span_parser.attach_program prog positioned
