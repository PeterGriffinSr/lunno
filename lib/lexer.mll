{
    open Token
    open Error
    let string_buffer = Buffer.create 64
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'
let identifier = ['a'-'z' 'A'-'Z' '_'](['a'-'z' 'A'-'Z' '_' '\''])*
let digits = ['0'-'9']+
let int_literal = digits ( '_' digits )*
let float_literal =
  digits ( '_' digits )*
  '.' digits ( '_' digits )+
  ( ['e' 'E'] ['+' '-']? digits ( '_' digits )* )?

rule token = parse
    | whitespace { token lexbuf }
    | newline { Lexing.new_line lexbuf; token lexbuf }
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
    | "function" { Function }
    | "if" { If }
    | "then" { Then }
    | "else" { Else }
    | "match" { Match }
    | "case" { Case }
    | "int" { IntegerType }
    | "float" { FloatingPointType }
    | "string" { StringType }
    | "bool" { BooleanType }
    | "unit" { UnitType }
    | int_literal as i {
        (try Integer (Int64.of_string i)
         with Failure _ -> 
            raise (LexerError {
            code = E_Lex_InvalidInt;
            msg  = "Invalid integer literal";
            pos  = Lexing.lexeme_start_p lexbuf;
        }))
    }
    | float_literal as f { 
        (try FloatingPoint (float_of_string f)
         with Failure _ -> 
           raise (LexerError {
            code = E_Lex_InvalidInt;
            msg  = "Invalid floating-point literal";
            pos  = Lexing.lexeme_start_p lexbuf;
        }))
    }
    | identifier as id { Identifier id }
    | '"' { 
      Buffer.clear string_buffer;
      read_string string_buffer lexbuf
    }
    | eof { EndOfFile }
    | _ as c {
        raise (LexerError {
            code = E_Lex_UnexpectedChar;
            msg  = Printf.sprintf "Unexpected character: %S" (String.make 1 c);
            pos  = Lexing.lexeme_start_p lexbuf;
        })
    }
and read_string buffer = parse
    | '"' { 
        let s = Buffer.contents buffer in
        if String.length s = 0 then
            raise (LexerError {
            code = E_Lex_EmptyString;
            msg  = "Empty string literals are not allowed";
            pos  = Lexing.lexeme_start_p lexbuf;
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
            | _ -> raise (LexerError {
                code = E_Lex_InvalidEscape;
                msg  = "Invalid escape sequence in string literal";
                pos  = Lexing.lexeme_start_p lexbuf;
            })
        in
        Buffer.add_char buffer c;
        read_string buffer lexbuf
    }
    | '\n' {  raise (LexerError {
        code = E_Lex_NewlineInString;
        msg  = "Newline in string literal";
        pos  = Lexing.lexeme_start_p lexbuf;
    }) }
    | eof { raise (LexerError {
        code = E_Lex_UnterminatedString;
        msg  = "Unterminated string literal";
        pos  = Lexing.lexeme_start_p lexbuf;
    })}
    | _ as c { Buffer.add_char buffer c; read_string buffer lexbuf }
and read_comment = parse
    | newline { Lexing.new_line lexbuf; token lexbuf }
    | eof { EndOfFile }
    | _ { read_comment lexbuf }