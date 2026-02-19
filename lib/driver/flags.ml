type t = { dump_program : bool; files : string list }

let version = Version.version

let parse argv =
  let dump_program = ref false in
  let files = ref [] in
  let specs =
    [
      ("--dump-program", Arg.Set dump_program, " Print the AST and exit");
      ( "--version",
        Arg.Unit
          (fun () ->
            Printf.printf "%s\n" version;
            exit 0),
        " Print version and exit" );
    ]
  in
  let usage = Printf.sprintf "Usage: %s <options> <files>" argv.(0) in
  (try Arg.parse_argv argv specs (fun f -> files := f :: !files) usage with
  | Arg.Help msg ->
      print_string msg;
      exit 0
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      exit 1);
  if !files = [] then (
    Printf.eprintf "No input files\n%!";
    exit 1);
  { dump_program = !dump_program; files = List.rev !files }
