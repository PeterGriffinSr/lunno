open Lunno

let unwrap lines = function
  | Ok v -> v
  | Error e ->
      Lunno_common.Error.print_error lines e;
      exit 1

let process_file dump_mode filename =
  let lines, prog_result = parse_file filename in
  match dump_mode with
  | Some Flags.DumpUntyped ->
      Debug.dump_program_untyped (unwrap lines prog_result)
  | Some Flags.DumpTyped ->
      let prog = unwrap lines prog_result in
      Debug.dump_program_typed (unwrap lines (typecheck_program lines prog))
  | None ->
      let prog = unwrap lines prog_result in
      let _ = unwrap lines (typecheck_program lines prog) in
      Printf.eprintf "error: compilation not yet implemented\n%!";
      exit 1

let () =
  let { Flags.subcommand; dump_mode; files } = Flags.parse Sys.argv in
  match subcommand with
  | Some Flags.Lsp -> run_lsp ()
  | None -> List.iter (process_file dump_mode) files
