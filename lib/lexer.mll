{
    open Token
    open Error
    let string_buffer = Buffer.create 64
    let reserved = [
        "let", Let;
        "function", Function;
        "if", If;
        "then", Then;
        "else", Else;
        "match", Match;
        "case", Case;
        "in", In;
        "int", IntegerType;
        "float", FloatingPointType;
        "string", StringType;
        "bool", BooleanType;
        "unit", UnitType;
    ]
}

let identifier = ['a'-'z' 'A'-'Z' '_'](['a'-'z' 'A'-'Z' '_' '\''])*
let digits = ['0'-'9' '_']+
let int_literal = digits+
let float_literal =
  digits '.' digits
  (['e' 'E'] ['+' '-']? digits)? 

rule token = parse
    | [' ' '\t' '\r']+ { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "#" { read_comment lexbuf }
    | "(" { LeftParen }
    | ")" { RightParen }
    | "{" { LeftBrace }
    | "}" { RightBrace }
    | "[" { LeftBracket }
    | "]" { RightBracket }
    | "+" { Plus }
    | "-" { Minus }
    | "*" { Asterisk }
    | "/" { Slash }
    | "=" { Equal }
    | "<>" { NotEqual }
    | "<" { Less }
    | ">" { Greater }
    | "," { Comma }
    | ":" { Colon }
    | "->" { Arrow }
    | float_literal as f { 
        (try FloatingPoint (float_of_string f)
         with Failure _ -> 
           raise (LexerError {
            code = E_Lex_InvalidInt;
            msg  = "Invalid floating-point literal";
            pos  = Lexing.lexeme_start_p lexbuf;
        }))
    }
    | int_literal as i {
        (try Integer (Int64.of_string i)
         with Failure _ -> 
            raise (LexerError {
            code = E_Lex_InvalidInt;
            msg  = "Invalid integer literal";
            pos  = Lexing.lexeme_start_p lexbuf;
        }))
    }
    | identifier as id { 
        match List.assoc_opt id reserved with
        | Some tok -> tok
        | None -> Identifier id
    }
    | '"' { 
      Buffer.clear string_buffer;
      let start_pos = Lexing.lexeme_start_p lexbuf in
      read_string string_buffer start_pos lexbuf
    }
    | eof { EndOfFile }
    | _ as c {
        raise (LexerError {
            code = E_Lex_UnexpectedChar;
            msg  = Printf.sprintf "Unexpected character: %S" (String.make 1 c);
            pos  = Lexing.lexeme_start_p lexbuf;
        })
    }
and read_string buffer start_pos = parse
    | '"' { 
        let s = Buffer.contents buffer in
        if String.length s = 0 then
            raise (LexerError {
            code = E_Lex_EmptyString;
            msg  = "Empty string literals are not allowed";
            pos  = start_pos;
        })
        else
            String s
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
      raise (LexerError {
        code = E_Lex_InvalidEscape;
        msg  = Printf.sprintf "Invalid escape sequence in string literal '\\%c'" c;
        pos  = Lexing.lexeme_start_p lexbuf;
      })
    }
    | '\n' {  
        raise (LexerError {
            code = E_Lex_NewlineInString;
            msg  = "Newline in string literal";
            pos  = Lexing.lexeme_start_p lexbuf;
        }) 
    }
    | eof { 
        raise (LexerError {
            code = E_Lex_UnterminatedString;
            msg  = "Unterminated string literal";
            pos  = start_pos;
        })
    }
    | _ as c { Buffer.add_char buffer c; read_string buffer start_pos lexbuf }
and read_comment = parse
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | eof { EndOfFile }
    | _ { read_comment lexbuf }