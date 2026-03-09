open Lunno_frontend
open Lunno_lower
open Lunno_common

let parse_expr s =
  let lexbuf = Lexing.from_string s in
  let prog = Parser.program Lexer.token lexbuf in
  match prog.body with
  | [ expr ] -> expr
  | _ -> failwith "Expected single expression"

let parse_program s =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

let assert_parses ~ctxt input =
  try
    let _ = parse_expr input in
    ()
  with e ->
    OUnit2.assert_failure
      (Printf.sprintf "Failed to parse: %s\nError: %s" input
         (Printexc.to_string e))

let assert_program_parses ~ctxt:_ input =
  try
    let _ = parse_program input in
    ()
  with e ->
    OUnit2.assert_failure
      (Printf.sprintf "Failed to parse program: %s\nError: %s" input
         (Printexc.to_string e))

let assert_parse_fails ~ctxt:_ input =
  match parse_expr input with
  | exception _ -> () (* any exception means it correctly rejected the input *)
  | _ ->
      OUnit2.assert_failure
        (Printf.sprintf "Expected parse failure but succeeded: %s" input)

let assert_program_parse_fails ~ctxt:_ input =
  match parse_program input with
  | exception _ -> ()
  | _ ->
      OUnit2.assert_failure
        (Printf.sprintf "Expected program parse failure but succeeded: %s" input)

let lex_all s =
  let lexbuf = Lexing.from_string s in
  let rec aux acc =
    match Lexer.token lexbuf with
    | Parser.EndOfFile _ -> List.rev acc
    | tok -> aux (tok :: acc)
  in
  aux []

let lex_exn s =
  let lexbuf = Lexing.from_string s in
  Lexer.token lexbuf

let assert_tokens ~ctxt input expected =
  let actual = lex_all input in
  OUnit2.assert_equal ~ctxt
    ~printer:(fun toks -> String.concat ", " toks)
    (expected |> List.map Token.to_string)
    (actual |> List.map Token.to_string)

let assert_lexer_error ~ctxt input expected_code =
  try
    let _ = lex_exn input in
    OUnit2.assert_failure "Expected LexerError, but none was raised"
  with Lexer.LexError e ->
    OUnit2.assert_equal ~ctxt ~printer:Error.string_of_code expected_code e.code

let check s =
  let lexbuf = Lexing.from_string s in
  match Lunno_driver.Cli.parse lexbuf with
  | Error e -> Error e
  | Ok prog -> Lunno_lower.Typechecker.infer_program prog

let assert_ok ~ctxt:_ s =
  match check s with
  | Ok _ -> ()
  | Error { Error.msg; _ } ->
      OUnit2.assert_failure
        (Printf.sprintf "Expected OK but got type error: %s\nInput: %s" msg s)

let assert_error ~ctxt:_ ?(code = None) s =
  match check s with
  | Ok _ ->
      OUnit2.assert_failure
        (Printf.sprintf "Expected type error but got none\nInput: %s" s)
  | Error { Error.code = actual_code; _ } -> (
      match code with
      | Some expected -> OUnit2.assert_equal expected actual_code
      | None -> ())
