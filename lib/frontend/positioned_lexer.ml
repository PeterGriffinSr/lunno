open Lunno_common

let build_pos_table src =
  let n = String.length src in
  let tbl = Array.make n Span.dummy_pos in
  let line = ref 1 in
  let col = ref 1 in
  for i = 0 to n - 1 do
    tbl.(i) <- { Span.line = !line; col = !col };
    if src.[i] = '\n' then begin
      incr line;
      col := 1
    end
    else incr col
  done;
  tbl

let token_extent src tok offset =
  let n = String.length src in
  let rec skip_ws i =
    if i >= n then i
    else
      match src.[i] with
      | ' ' | '\t' | '\r' | '\n' -> skip_ws (i + 1)
      | '#' ->
          let rec skip_line j =
            if j >= n || src.[j] = '\n' then skip_ws (j + 1)
            else skip_line (j + 1)
          in
          skip_line (i + 1)
      | _ -> i
  in
  let start = skip_ws offset in
  let stop =
    match tok with
    | Lunno.TEOF -> start
    | Lunno.TIntLit _ ->
        let j = ref start in
        while !j < n && src.[!j] >= '0' && src.[!j] <= '9' do
          incr j
        done;
        if
          !j + 1 < n
          && src.[!j] = '.'
          && src.[!j + 1] >= '0'
          && src.[!j + 1] <= '9'
        then begin
          incr j;
          while !j < n && src.[!j] >= '0' && src.[!j] <= '9' do
            incr j
          done
        end;
        !j
    | Lunno.TFloatLit _ ->
        let j = ref start in
        while !j < n && src.[!j] >= '0' && src.[!j] <= '9' do
          incr j
        done;
        if !j < n && src.[!j] = '.' then begin
          incr j;
          while !j < n && src.[!j] >= '0' && src.[!j] <= '9' do
            incr j
          done
        end;
        !j
    | Lunno.TStringLit _ ->
        let j = ref (start + 1) in
        while !j < n && src.[!j] <> '"' do
          if src.[!j] = '\\' then incr j;
          incr j
        done;
        !j + 1
    | Lunno.TIdent s ->
        let _ = s in
        let j = ref start in
        let is_ident_char c =
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '_'
        in
        while !j < n && is_ident_char src.[!j] do
          incr j
        done;
        !j
    | Lunno.TArrow | Lunno.TNotEq | Lunno.TCons | Lunno.TDotDot -> start + 2
    | _ ->
        let j = ref start in
        let is_ident_char c =
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '_'
        in
        if start < n && is_ident_char src.[start] then begin
          while !j < n && is_ident_char src.[!j] do
            incr j
          done
        end
        else incr j;
        !j
  in
  (start, stop)

let position_tokens src tokens =
  let tbl = build_pos_table src in
  let n = String.length src in
  let pos_of i =
    if i < n then tbl.(i)
    else { Span.line = tbl.(n - 1).Span.line; col = tbl.(n - 1).Span.col + 1 }
  in
  let offset = ref 0 in
  List.map
    (fun tok ->
      let lo, hi = token_extent src tok !offset in
      let sp = { Span.start = pos_of lo; stop = pos_of hi } in
      offset := hi;
      (tok, sp))
    tokens

let lex src =
  match Lunno.lex (Util.ocaml_string_to_coq src) with
  | Lunno.LexErr msg -> Error (Util.coq_string_to_string msg)
  | Lunno.LexOk tokens -> Ok (position_tokens src tokens)
