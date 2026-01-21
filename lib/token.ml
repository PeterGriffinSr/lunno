type token =
  | Integer of int64
  | FloatingPoint of float
  | String of string
  | Identifier of string
  | Function
  | If
  | Then
  | Else
  | Match
  | Case
  | IntegerType
  | FloatingPointType
  | StringType
  | BooleanType
  | UnitType
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Equal
  | NotEqual
  | Less
  | Greater
  | Comma
  | Colon
  | Arrow
  | EndOfFile

let to_string = function
  | Integer i -> Printf.sprintf "Integer(%Ld)" i
  | FloatingPoint f -> Printf.sprintf "Float(%g)" f
  | String s -> Printf.sprintf "String(%S)" s
  | Identifier id -> Printf.sprintf "Identifier(%s)" id
  | Function -> "Function"
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Match -> "Match"
  | Case -> "Case"
  | IntegerType -> "IntegerType"
  | FloatingPointType -> "FloatingPointType"
  | StringType -> "StringType"
  | BooleanType -> "BooleanType"
  | UnitType -> "UnitType"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Slash -> "/"
  | Equal -> "="
  | NotEqual -> "<>"
  | Less -> "<"
  | Greater -> ">"
  | Comma -> ","
  | Colon -> ":"
  | Arrow -> "->"
  | EndOfFile -> "EOF"
