open Lunno

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "lsp" then run_lsp ()
  else
    let flags = Flags.parse Sys.argv in
    List.iter
      (fun filename ->
        let lines, prog = parse_file filename in
        let typed_prog = typecheck_program lines prog in
        if flags.Flags.dump_program then Debug.dump_program typed_prog)
      flags.Flags.files
