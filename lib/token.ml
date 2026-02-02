type span = Lexing.position * Lexing.position

type t =
  | Integer of int64 * span
  | FloatingPoint of float * span
  | String of string * span
  | Identifier of string * span
  | Let of span
  | Function of span
  | If of span
  | Then of span
  | Else of span
  | Match of span
  | Case of span
  | In of span
  | IntegerType of span
  | FloatingPointType of span
  | StringType of span
  | BooleanType of span
  | UnitType of span
  | LeftParen of span
  | RightParen of span
  | LeftBrace of span
  | RightBrace of span
  | LeftBracket of span
  | RightBracket of span
  | Plus of span
  | Minus of span
  | Asterisk of span
  | Slash of span
  | Equal of span
  | NotEqual of span
  | Less of span
  | Greater of span
  | Comma of span
  | Colon of span
  | Arrow of span
  | EndOfFile of span

let to_string = function
  | Integer (i, _) -> Printf.sprintf "Integer(%Ld)" i
  | FloatingPoint (f, _) -> Printf.sprintf "Float(%g)" f
  | String (s, _) -> Printf.sprintf "String(%S)" s
  | Identifier (id, _) -> Printf.sprintf "Identifier(%s)" id
  | Let _ -> "Let"
  | Function _ -> "Function"
  | If _ -> "If"
  | Then _ -> "Then"
  | Else _ -> "Else"
  | Match _ -> "Match"
  | Case _ -> "Case"
  | In _ -> "In"
  | IntegerType _ -> "IntegerType"
  | FloatingPointType _ -> "FloatingPointType"
  | StringType _ -> "StringType"
  | BooleanType _ -> "BooleanType"
  | UnitType _ -> "UnitType"
  | LeftParen _ -> "("
  | RightParen _ -> ")"
  | LeftBrace _ -> "{"
  | RightBrace _ -> "}"
  | LeftBracket _ -> "["
  | RightBracket _ -> "]"
  | Plus _ -> "+"
  | Minus _ -> "-"
  | Asterisk _ -> "*"
  | Slash _ -> "/"
  | Equal _ -> "="
  | NotEqual _ -> "<>"
  | Less _ -> "<"
  | Greater _ -> ">"
  | Comma _ -> ","
  | Colon _ -> ":"
  | Arrow _ -> "->"
  | EndOfFile _ -> "EOF"
