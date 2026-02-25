{
    open Parser
    open Lunno_common
    
    let string_buffer = Buffer.create 64
    let reserved = 
        Hashtbl.of_seq (List.to_seq [
        "let", (fun  span -> KwLet span);
        "if", (fun span -> KwIf span);
        "then", (fun span -> KwThen span);
        "else", (fun span -> KwElse span);
        "match", (fun span -> KwMatch span);
        "import", (fun span -> KwImport span);
        "int", (fun span -> IntegerType span);
        "float", (fun span -> FloatingPointType span);
        "string", (fun span -> StringType span);
        "bool", (fun span -> BooleanType span);
        "unit", (fun span -> UnitType span);
        "true", (fun span -> Boolean (true, span));
        "false", (fun span -> Boolean (false, span));
    ])
    let strip_underscores s = 
        let buffer = Buffer.create (String.length s) in
        String.iter (fun c -> 
            if c <> '_' then Buffer.add_char buffer c
        ) s;
        Buffer.contents buffer
    let with_pos lexbuf ctor =
        let start = Lexing.lexeme_start_p lexbuf in
        let stop = Lexing.lexeme_end_p lexbuf in
        ctor (start, stop)
}

let identifier = ['a'-'z' 'A'-'Z' '_'](['a'-'z' 'A'-'Z' '_' '0'-'9' '\''])*
let digits = ['0'-'9']+
let int_literal = digits ('_' digits)*
let float_literal =
  digits '.' digits
  (['e' 'E'] ['+' '-']? digits)? 

rule token = parse
    | [' ' '\t' '\r']+ { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "#" { read_comment lexbuf }
    | "(" { with_pos lexbuf (fun span -> LeftParen span) }
    | ")" { with_pos lexbuf (fun span -> RightParen span) }
    | "{" { with_pos lexbuf (fun span -> LeftBrace span) }
    | "}" { with_pos lexbuf (fun span -> RightBrace span) }
    | "[" { with_pos lexbuf (fun span -> LeftBracket span) }
    | "]" { with_pos lexbuf (fun span -> RightBracket span) }
    | "+" { with_pos lexbuf (fun span -> Plus span) }
    | "-" { with_pos lexbuf (fun span -> Minus span) }
    | "*" { with_pos lexbuf (fun span -> Asterisk span) }
    | "/" { with_pos lexbuf (fun span -> Slash span) }
    | "->" { with_pos lexbuf (fun span -> Arrow span) }
    | "<>" { with_pos lexbuf (fun span -> NotEqual span) }
    | "::" { with_pos lexbuf (fun span -> Cons span) }
    | "=" { with_pos lexbuf (fun span -> Equal span) }
    | "<" { with_pos lexbuf (fun span -> Less span) }
    | ">" { with_pos lexbuf (fun span -> Greater span) }
    | "," { with_pos lexbuf (fun span -> Comma span) }
    | ":" { with_pos lexbuf (fun span -> Colon span) }
    | "|" { with_pos lexbuf (fun span -> Pipe span) }
    | "_" { with_pos lexbuf (fun span -> Underscore span) }
    | "." { with_pos lexbuf (fun span -> Dot span) }
    | ".." { with_pos lexbuf (fun span -> DotDot span) }
    | float_literal as f {
        with_pos lexbuf (fun span ->
            try FloatingPoint (float_of_string (strip_underscores f), span)
            with Failure _ ->
                raise (Error.LexerError {
                    code = Error.E_Lex_InvalidFloat;
                    msg  = "Invalid floating-point literal";
                    span = span;
                }))
    }
    | digits '_' ([^'0'-'9'] | eof) {
        with_pos lexbuf (fun span ->
            raise (Error.LexerError {
                code = Error.E_Lex_InvalidInt;
                msg = "Trailing underscore in integer literal";
                span;
            }))
    }
    | int_literal as i {
        with_pos lexbuf (fun span ->
            try Integer (Int64.of_string (strip_underscores i), span)
            with Failure _ ->
                raise (Error.LexerError {
                    code = Error.E_Lex_InvalidInt;
                    msg  = "Invalid integer literal";
                    span = span;
                }))
    }
    | identifier as id { 
        match Hashtbl.find_opt reserved id with
        | Some ctor -> with_pos lexbuf ctor
        | None -> with_pos lexbuf (fun span -> Identifier (id, span))
    }
    | '"' { 
      Buffer.clear string_buffer;
      let start_pos = Lexing.lexeme_start_p lexbuf in
      read_string string_buffer start_pos lexbuf
    }
    | eof { let pos = Lexing.lexeme_start_p lexbuf in 
        EndOfFile (pos, pos) }
    | _ as c {
        with_pos lexbuf (fun span ->
            raise (Error.LexerError {
                code = Error.E_Lex_UnexpectedChar;
                msg  = Printf.sprintf "Unexpected character: %S" (String.make 1 c);
                span = span;
            }))
    }
and read_string buffer start_pos = parse
    | '"' { 
        with_pos lexbuf (fun end_span ->
            let s = Buffer.contents buffer in
            if String.length s = 0 then 
                raise (Error.LexerError {
                    code = Error.E_Lex_EmptyString;
                    msg  = "Empty string literals are not allowed";
                    span = (start_pos, snd end_span);
                })
            else
                String (s, end_span))
    }
    | '\\' (['\\' 'n' 't' 'r' '"' '\''] as c) {
        let c = match c with
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | '"' -> '"'
            | '\'' -> '\''
            | '\\' -> '\\'
            | _ -> assert false
        in
        Buffer.add_char buffer c;
        read_string buffer start_pos lexbuf
    }
    | '\\' (_ as c) {
      raise (Error.LexerError {
        code = Error.E_Lex_InvalidEscape;
        msg  = Printf.sprintf "Invalid escape sequence in string literal '\\%c'" c;
        span = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
      })
    }
    | '\n' {  
        raise (Error.LexerError {
            code = Error.E_Lex_NewlineInString;
            msg  = "Newline in string literal";
            span = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
        }) 
    }
    | eof { 
        raise (Error.LexerError {
            code = Error.E_Lex_UnterminatedString;
            msg  = "Unterminated string literal";
            span = (start_pos, Lexing.lexeme_end_p lexbuf);
        })
    }
    | _ as c { Buffer.add_char buffer c; read_string buffer start_pos lexbuf }
and read_comment = parse
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | eof { token lexbuf }
    | _ { read_comment lexbuf }