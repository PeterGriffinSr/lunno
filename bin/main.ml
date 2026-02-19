open Lunno

let () =
  let flags = Flags.parse Sys.argv in
  List.iter
    (fun filename ->
      let prog = parse_file filename in
      if flags.dump_program then Debug.dump_program prog)
    flags.files
