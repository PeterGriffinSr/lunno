open Lunno

let () =
  match Array.to_list Sys.argv with
  | [ _; filename ] ->
      let prog = parse_file filename in
      Debug.dump_program prog
  | _ ->
      Printf.eprintf "Usage: %s <filename>\n%!" Sys.argv.(0);
      exit 1
