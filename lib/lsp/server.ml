open Lsp
open Lsp.Types
open Jsonrpc

let documents : (string, string) Hashtbl.t = Hashtbl.create 16

let typed_asts : (string, Lunno_common.Typed_ast.program) Hashtbl.t =
  Hashtbl.create 16

let send_packet packet =
  let json = Packet.yojson_of_t packet in
  let body = Yojson.Safe.to_string json in
  Printf.printf "Content-Length: %d\r\n\r\n%s%!" (String.length body) body

let send_response id result =
  send_packet (Packet.Response (Response.ok id result))

let send_notification notif =
  send_packet (Packet.Notification (Server_notification.to_jsonrpc notif))

let read_message () =
  try
    let content_length = ref 0 in
    let rec read_headers () =
      let line = input_line stdin in
      let line = String.trim line in
      if line = "" then ()
      else begin
        if String.length line > 16 && String.sub line 0 16 = "Content-Length: "
        then
          content_length :=
            int_of_string
              (String.trim (String.sub line 16 (String.length line - 16)));
        read_headers ()
      end
    in
    read_headers ();
    let buf = Bytes.create !content_length in
    really_input stdin buf 0 !content_length;
    Some (Bytes.to_string buf)
  with End_of_file -> None

let span_to_range (start_pos, end_pos) =
  Range.create
    ~start:
      (Position.create
         ~line:(start_pos.Lexing.pos_lnum - 1)
         ~character:(start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol))
    ~end_:
      (Position.create
         ~line:(end_pos.Lexing.pos_lnum - 1)
         ~character:(end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol))

let publish_diagnostics uri diagnostics =
  let params = PublishDiagnosticsParams.create ~uri ~diagnostics () in
  send_notification (Server_notification.PublishDiagnostics params)

let check_document uri source =
  Lunno_modules.Core.Init.init ();
  let uri_str = Uri.to_string uri in
  let diagnostics =
    try
      let lexbuf = Lexing.from_string source in
      Lexing.set_filename lexbuf uri_str;
      let prog =
        Lunno_frontend.Parser.program Lunno_frontend.Lexer.token lexbuf
      in
      try
        let typed = Lunno_lower.Typechecker.infer_program prog in
        Hashtbl.replace typed_asts uri_str typed;
        []
      with Lunno_common.Error.TypeError { msg; span; _ } ->
        [
          Diagnostic.create ~range:(span_to_range span)
            ~severity:DiagnosticSeverity.Error ~message:(`String msg) ();
        ]
    with
    | Lunno_common.Error.ParseError { msg; span; _ } ->
        [
          Diagnostic.create ~range:(span_to_range span)
            ~severity:DiagnosticSeverity.Error ~message:(`String msg) ();
        ]
    | Lunno_common.Error.LexerError { msg; span; _ } ->
        [
          Diagnostic.create ~range:(span_to_range span)
            ~severity:DiagnosticSeverity.Error ~message:(`String msg) ();
        ]
    | _ -> []
  in
  publish_diagnostics uri diagnostics

let string_of_ty_sig ty =
  Lunno_common.Ty_utils.string_of_ty
    (Lunno_common.Ty_utils.generalize_display ty)

let rec collect_code_lenses acc expr =
  let open Lunno_common.Typed_ast in
  match expr with
  | Let { name = _; let_ty; let_body; let_span; _ } ->
      let start_pos, _ = let_span in
      let line = start_pos.Lexing.pos_lnum - 1 in
      let range =
        Range.create
          ~start:(Position.create ~line ~character:0)
          ~end_:(Position.create ~line ~character:0)
      in
      let display_ty =
        match let_body with Lambda { lambda_ty; _ } -> lambda_ty | _ -> let_ty
      in
      let label = string_of_ty_sig display_ty in
      let lens =
        CodeLens.create ~range
          ~command:(Command.create ~title:label ~command:"" ())
          ()
      in
      collect_code_lenses (lens :: acc) let_body
  | Block (exprs, _, _) -> List.fold_left collect_code_lenses acc exprs
  | _ -> acc

let handle_code_lens id (params : CodeLensParams.t) =
  let uri = params.CodeLensParams.textDocument.TextDocumentIdentifier.uri in
  let uri_str = Uri.to_string uri in
  match Hashtbl.find_opt typed_asts uri_str with
  | None -> send_response id (`List [])
  | Some typed_prog ->
      let lenses =
        List.fold_left collect_code_lenses []
          typed_prog.Lunno_common.Typed_ast.body
        |> List.rev
      in
      let json = `List (List.map CodeLens.yojson_of_t lenses) in
      send_response id json

let extract_word_at_position lines line char_pos =
  if line < 0 || line >= List.length lines then ("", "")
  else
    let current_line = List.nth lines line in
    let line_len = String.length current_line in
    let char_pos = min char_pos line_len in

    let is_ident_char c =
      (Char.code c >= 48 && Char.code c <= 57)
      || (Char.code c >= 65 && Char.code c <= 90)
      || (Char.code c >= 97 && Char.code c <= 122)
      || c = '_' || c = '\''
    in
    let rec find_start pos =
      if pos < 0 then 0
      else if is_ident_char current_line.[pos] then find_start (pos - 1)
      else pos + 1
    in
    let rec find_end pos =
      if pos >= line_len then pos
      else if is_ident_char current_line.[pos] then find_end (pos + 1)
      else pos
    in
    let start_pos = find_start (char_pos - 1) in
    let end_pos = find_end char_pos in
    let word =
      if start_pos < end_pos then
        String.sub current_line start_pos (end_pos - start_pos)
      else ""
    in
    if start_pos > 0 && current_line.[start_pos - 1] = '.' then
      let rec find_module_start pos =
        if pos < 0 then 0
        else
          let c = current_line.[pos] in
          if is_ident_char c || c = '.' then find_module_start (pos - 1)
          else pos + 1
      in
      let mod_start = find_module_start (start_pos - 2) in
      let module_path =
        String.sub current_line mod_start (start_pos - mod_start - 1)
      in
      (module_path, word)
    else ("", word)

let rec collect_identifiers acc expr =
  let open Lunno_common.Typed_ast in
  match expr with
  | Let { name; let_ty; let_body; _ } ->
      let acc = (name, string_of_ty_sig let_ty) :: acc in
      collect_identifiers acc let_body
  | Block (exprs, _, _) -> List.fold_left collect_identifiers acc exprs
  | Lambda { params; lambda_body; _ } ->
      let acc =
        List.fold_left
          (fun acc param ->
            (param.param_name, string_of_ty_sig param.param_ty) :: acc)
          acc params
      in
      collect_identifiers acc lambda_body
  | _ -> acc

let extract_imports source =
  let lines = String.split_on_char '\n' source in
  List.filter_map
    (fun line ->
      let line = String.trim line in
      if String.starts_with ~prefix:"import" line then
        try
          let after_import =
            String.trim (String.sub line 6 (String.length line - 6))
          in
          let after_quote =
            String.trim
              (String.sub after_import 1 (String.length after_import - 2))
          in
          match String.split_on_char ':' after_quote with
          | [ _namespace; module_name ] -> Some module_name
          | _ -> None
        with _ -> None
      else None)
    lines

let handle_completion id (params : CompletionParams.t) =
  let uri = params.CompletionParams.textDocument.TextDocumentIdentifier.uri in
  let uri_str = Uri.to_string uri in
  let position = params.CompletionParams.position in
  let line = position.Position.line in
  let char_pos = position.Position.character in

  match Hashtbl.find_opt documents uri_str with
  | None -> send_response id (`List [])
  | Some source ->
      let lines = String.split_on_char '\n' source in
      let module_name, word = extract_word_at_position lines line char_pos in
      let imported_modules = extract_imports source in

      let items =
        match Hashtbl.find_opt typed_asts uri_str with
        | None -> []
        | Some typed_prog -> (
            let all_idents =
              List.fold_left collect_identifiers []
                typed_prog.Lunno_common.Typed_ast.body
              |> List.rev
            in
            if module_name = "" then
              let ident_items =
                List.filter_map
                  (fun (name, ty) ->
                    if
                      String.length name >= String.length word
                      && String.equal
                           (String.sub name 0 (String.length word))
                           word
                    then
                      let item =
                        CompletionItem.create ~label:name
                          ~kind:CompletionItemKind.Variable ~detail:ty ()
                      in
                      Some item
                    else None)
                  all_idents
              in
              let module_items =
                List.filter_map
                  (fun mod_name ->
                    if
                      String.length mod_name >= String.length word
                      && String.equal
                           (String.sub mod_name 0 (String.length word))
                           word
                    then
                      let item =
                        CompletionItem.create ~label:mod_name
                          ~kind:CompletionItemKind.Module ()
                      in
                      Some item
                    else None)
                  imported_modules
              in

              ident_items @ module_items
            else
              let namespace, name =
                match String.split_on_char '.' module_name with
                | [] -> ("core", "")
                | [ single ] -> ("core", single)
                | parts ->
                    let name = List.nth parts (List.length parts - 1) in
                    let namespace =
                      String.concat "."
                        (List.filteri
                           (fun i _ -> i < List.length parts - 1)
                           parts)
                    in
                    (namespace, name)
              in
              if name = "" then []
              else
                match Lunno_modules.Registry.find_module ~namespace ~name with
                | None -> []
                | Some module_info ->
                    List.filter_map
                      (fun (member_name, member_ty) ->
                        let matches =
                          if word = "" then true
                          else
                            String.length member_name >= String.length word
                            && String.equal
                                 (String.sub member_name 0 (String.length word))
                                 word
                        in
                        if matches then
                          let ty_str =
                            Lunno_common.Ty_utils.string_of_ty member_ty
                          in
                          let item =
                            CompletionItem.create ~label:member_name
                              ~kind:CompletionItemKind.Method ~detail:ty_str ()
                          in
                          Some item
                        else None)
                      module_info.Lunno_modules.Registry.exports)
      in

      let json = `List (List.map CompletionItem.yojson_of_t items) in
      send_response id json

let handle_hover id (params : HoverParams.t) =
  let uri = params.HoverParams.textDocument.TextDocumentIdentifier.uri in
  let uri_str = Uri.to_string uri in
  let position = params.HoverParams.position in
  let line = position.Position.line in
  let char_pos = position.Position.character in

  match Hashtbl.find_opt documents uri_str with
  | None -> send_response id `Null
  | Some source -> (
      let lines = String.split_on_char '\n' source in
      let module_path, word = extract_word_at_position lines line char_pos in
      match Hashtbl.find_opt typed_asts uri_str with
      | None -> send_response id `Null
      | Some typed_prog -> (
          let make_hover ty_str =
            let contents =
              MarkupContent.create ~kind:MarkupKind.Markdown
                ~value:(Printf.sprintf "```\n%s\n```" ty_str)
            in
            let hover = Hover.create ~contents:(`MarkupContent contents) () in
            send_response id (Hover.yojson_of_t hover)
          in
          if module_path <> "" then
            let namespace, name =
              match String.split_on_char '.' module_path with
              | [] -> ("core", "")
              | [ single ] -> ("core", single)
              | parts ->
                  let name = List.nth parts (List.length parts - 1) in
                  let namespace =
                    String.concat "."
                      (List.filteri
                         (fun i _ -> i < List.length parts - 1)
                         parts)
                  in
                  (namespace, name)
            in
            match Lunno_modules.Registry.find_module ~namespace ~name with
            | None -> send_response id `Null
            | Some module_info -> (
                match
                  List.assoc_opt word module_info.Lunno_modules.Registry.exports
                with
                | None -> send_response id `Null
                | Some member_ty ->
                    make_hover (Lunno_common.Ty_utils.string_of_ty member_ty))
          else
            let imported_modules = extract_imports source in
            if List.mem word imported_modules then
              match
                Lunno_modules.Registry.find_module ~namespace:"core" ~name:word
              with
              | None -> send_response id `Null
              | Some module_info ->
                  let exports_str =
                    module_info.Lunno_modules.Registry.exports
                    |> List.map (fun (n, ty) ->
                        Printf.sprintf "%s : %s" n
                          (Lunno_common.Ty_utils.string_of_ty ty))
                    |> String.concat "\n"
                  in
                  make_hover (Printf.sprintf "%s" exports_str)
            else
              let all_idents =
                List.fold_left collect_identifiers []
                  typed_prog.Lunno_common.Typed_ast.body
                |> List.rev
              in
              match List.assoc_opt word all_idents with
              | None -> send_response id `Null
              | Some ty_str -> make_hover ty_str))

let rec collect_definitions acc expr =
  let open Lunno_common.Typed_ast in
  match expr with
  | Let { name; let_span; let_body; _ } ->
      let acc = (name, let_span) :: acc in
      collect_definitions acc let_body
  | Block (exprs, _, _) -> List.fold_left collect_definitions acc exprs
  | Lambda { params; lambda_body; _ } ->
      let acc =
        List.fold_left
          (fun acc param -> (param.param_name, param.param_span) :: acc)
          acc params
      in
      collect_definitions acc lambda_body
  | _ -> acc

let handle_goto_definition id (params : DefinitionParams.t) =
  let uri = params.DefinitionParams.textDocument.TextDocumentIdentifier.uri in
  let uri_str = Uri.to_string uri in
  let position = params.DefinitionParams.position in
  let line = position.Position.line in
  let char_pos = position.Position.character in

  match Hashtbl.find_opt documents uri_str with
  | None -> send_response id `Null
  | Some source -> (
      let lines = String.split_on_char '\n' source in
      let _, word = extract_word_at_position lines line char_pos in
      match Hashtbl.find_opt typed_asts uri_str with
      | None -> send_response id `Null
      | Some typed_prog -> (
          let all_defs =
            List.fold_left collect_definitions []
              typed_prog.Lunno_common.Typed_ast.body
            |> List.rev
          in
          match List.assoc_opt word all_defs with
          | None -> send_response id `Null
          | Some span ->
              let range = span_to_range span in
              let location = Location.create ~uri ~range in
              send_response id (Location.yojson_of_t location)))

let handle_initialize id _params =
  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:(`TextDocumentSyncKind TextDocumentSyncKind.Full)
      ~hoverProvider:(`Bool true)
      ~codeLensProvider:(CodeLensOptions.create ~resolveProvider:false ())
      ~completionProvider:(CompletionOptions.create ~resolveProvider:false ())
      ~definitionProvider:(`Bool true) ()
  in
  let result =
    InitializeResult.create ~capabilities
      ~serverInfo:
        (InitializeResult.create_serverInfo ~name:"lunno-lsp" ~version:"0.8.0"
           ())
      ()
  in
  send_response id (InitializeResult.yojson_of_t result)

let handle_did_open (params : DidOpenTextDocumentParams.t) =
  let uri =
    params.DidOpenTextDocumentParams.textDocument.TextDocumentItem.uri
  in
  let source =
    params.DidOpenTextDocumentParams.textDocument.TextDocumentItem.text
  in
  Hashtbl.replace documents (Uri.to_string uri) source;
  check_document uri source

let handle_did_change (params : DidChangeTextDocumentParams.t) =
  let uri =
    params.DidChangeTextDocumentParams.textDocument
      .VersionedTextDocumentIdentifier.uri
  in
  match params.DidChangeTextDocumentParams.contentChanges with
  | [] -> ()
  | change :: _ ->
      let source = change.TextDocumentContentChangeEvent.text in
      Hashtbl.replace documents (Uri.to_string uri) source;
      check_document uri source

let handle_did_close (params : DidCloseTextDocumentParams.t) =
  let uri =
    params.DidCloseTextDocumentParams.textDocument.TextDocumentIdentifier.uri
  in
  Hashtbl.remove documents (Uri.to_string uri);
  publish_diagnostics uri []

let dispatch_request raw_req =
  let id = raw_req.Request.id in
  match Client_request.of_jsonrpc raw_req with
  | Error _ -> send_response id `Null
  | Ok (Client_request.E req) -> (
      match req with
      | Client_request.Initialize params -> handle_initialize id params
      | Client_request.Shutdown -> send_response id `Null
      | Client_request.TextDocumentCodeLens params -> handle_code_lens id params
      | Client_request.TextDocumentHover params -> handle_hover id params
      | Client_request.TextDocumentCompletion params ->
          handle_completion id params
      | Client_request.TextDocumentDefinition params ->
          handle_goto_definition id params
      | _ -> send_response id `Null)

let dispatch_notification (notif : Client_notification.t) =
  match notif with
  | Client_notification.TextDocumentDidOpen params -> handle_did_open params
  | Client_notification.TextDocumentDidChange params -> handle_did_change params
  | Client_notification.TextDocumentDidClose params -> handle_did_close params
  | Client_notification.Initialized -> ()
  | Client_notification.Exit -> exit 0
  | _ -> ()

let run () =
  let rec loop () =
    match read_message () with
    | None -> ()
    | Some body ->
        (try
           let json = Yojson.Safe.from_string body in
           match Packet.t_of_yojson json with
           | Packet.Request req -> dispatch_request req
           | Packet.Notification notif -> (
               match Client_notification.of_jsonrpc notif with
               | Ok notif -> dispatch_notification notif
               | Error _ -> ())
           | Packet.Response _ -> ()
           | Packet.Batch_call _ | Packet.Batch_response _ -> ()
         with _ -> ());
        loop ()
  in
  loop ()
