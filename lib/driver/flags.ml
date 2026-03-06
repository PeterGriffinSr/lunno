type dump_mode = DumpUntyped | DumpTyped
type subcommand = Lsp

type t = {
  subcommand : subcommand option;
  dump_mode : dump_mode option;
  files : string list;
}

let lsp_flag =
  let i = Cmdliner.Arg.info [ "lsp" ] ~doc:"Run the LSP server." in
  Cmdliner.Arg.(value & flag & i)

let dump_untyped_flag =
  let i =
    Cmdliner.Arg.info [ "dump-untyped" ] ~doc:"Print the untyped AST and exit."
  in
  Cmdliner.Arg.(value & flag & i)

let dump_typed_flag =
  let i =
    Cmdliner.Arg.info [ "dump-typed" ] ~doc:"Print the typed AST and exit."
  in
  Cmdliner.Arg.(value & flag & i)

let files_arg =
  let i =
    Cmdliner.Arg.info [] ~docv:"FILE" ~doc:"Input source file(s) to compile."
  in
  Cmdliner.Arg.(value & pos_all file [] & i)

let make_t lsp dump_untyped dump_typed files =
  let subcommand = if lsp then Some Lsp else None in
  let dump_mode =
    match (dump_untyped, dump_typed) with
    | true, _ -> Some DumpUntyped
    | _, true -> Some DumpTyped
    | _ -> None
  in
  { subcommand; dump_mode; files }

let term =
  Cmdliner.Term.(
    const make_t $ lsp_flag $ dump_untyped_flag $ dump_typed_flag $ files_arg)

let root_info =
  Cmdliner.Cmd.info "lunno" ~version:Version.version ~doc:"The Lunno compiler."
    ~exits:[ Cmdliner.Cmd.Exit.info ~docs:Cmdliner.Manpage.s_none 0 ]

let root = Cmdliner.Cmd.v root_info term

let parse _argv =
  match Cmdliner.Cmd.eval_value root with
  | Ok (`Ok t) -> t
  | Ok `Help | Ok `Version -> exit 0
  | Error _ -> exit 1
