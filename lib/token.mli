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

val to_string : token -> string
