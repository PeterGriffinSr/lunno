open Lunno

let () =
  let flags = Flags.parse Sys.argv in
  List.iter
    (fun filename ->
      let lines, prog = parse_file filename in
      typecheck_program lines prog;
      if flags.Flags.dump_program then Debug.dump_program prog)
    flags.Flags.files
