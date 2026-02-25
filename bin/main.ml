open Lunno

let () =
  let flags = Flags.parse Sys.argv in
  List.iter
    (fun filename ->
      let lines, prog = parse_file filename in
      let typed_prog = typecheck_program lines prog in
      if flags.Flags.dump_program then Debug.dump_program typed_prog)
    flags.Flags.files
