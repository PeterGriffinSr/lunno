open Lunno_driver
open Lunno_debug

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  if args = [] then begin
    Printf.eprintf "Usage: lunno <file.ln> [...]\n";
    exit 1
  end;
  List.iter
    (fun path ->
      let prog = Cli.parse_file path in
      Debug.dump_program prog)
    args
