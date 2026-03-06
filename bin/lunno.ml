open Lunno

let process_file dump_mode filename =
  let lines, prog = parse_file filename in
  match dump_mode with
  | Some Flags.DumpUntyped -> Debug.dump_program_untyped prog
  | Some Flags.DumpTyped ->
      Debug.dump_program_typed (typecheck_program lines prog)
  | None ->
      let _ = typecheck_program lines prog in
      Printf.eprintf "error: compilation not yet implemented\n%!";
      exit 1

let () =
  let { Flags.subcommand; dump_mode; files } = Flags.parse Sys.argv in
  match subcommand with
  | Some Flags.Lsp -> run_lsp ()
  | None -> List.iter (process_file dump_mode) files
