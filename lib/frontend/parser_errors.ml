(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
 fun s ->
  match s with
  | 191 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 189 ->
      "Syntax error: unexpected token after import; expected another 'import' \
       or an expression.\n"
  | 187 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 181 ->
      "Syntax error: expected a string literal (module path) after 'import'.\n"
  | 179 -> "Syntax error: expected ')' to close parenthesized expression.\n"
  | 177 ->
      "Syntax error: unexpected token after expression; expected another \
       expression or end of block.\n"
  | 175 -> "Syntax error: expected '}' to close block expression.\n"
  | 171 ->
      "Syntax error: unexpected token after match case; expected '|' or '}'.\n"
  | 169 -> "Syntax error: expected an expression after '->' in match case.\n"
  | 167 -> "Syntax error: expected an expression after '->' in match case.\n"
  | 166 -> "Syntax error: expected '->' after guard expression in match case.\n"
  | 165 ->
      "Syntax error: expected a guard expression after 'if' in match case.\n"
  | 164 -> "Syntax error: expected '->' or 'if' after pattern in match case.\n"
  | 161 -> "Syntax error: expected ')' to close parenthesized pattern.\n"
  | 159 -> "Syntax error: expected a pattern after '::' in cons pattern.\n"
  | 158 ->
      "Syntax error: unexpected token in match pattern; expected '::', '->', \
       or 'if'.\n"
  | 152 ->
      "Syntax error: expected ']' for empty list pattern or a pattern element \
       after '['.\n"
  | 151 -> "Syntax error: expected a pattern after '(' in match case.\n"
  | 148 -> "Syntax error: expected a pattern after '|' in match case.\n"
  | 147 -> "Syntax error: expected '|' to begin match cases.\n"
  | 146 -> "Syntax error: expected '{' after match expression.\n"
  | 143 -> "Syntax error: expected '{' to begin function body.\n"
  | 142 ->
      "Syntax error: expected a return type after '->' in 'let' definition.\n"
  | 140 -> "Syntax error: expected an expression after '=' in 'let' binding.\n"
  | 139 ->
      "Syntax error: expected '=' after type annotation in 'let' binding.\n"
  | 138 -> "Syntax error: expected ',' or ')' in type list.\n"
  | 136 -> "Syntax error: expected a type expression after ',' in type list.\n"
  | 134 -> "Syntax error: expected ')', ',', or '->' in type list.\n"
  | 132 ->
      "Syntax error: expected a type expression after '->' in tuple type.\n"
  | 131 -> "Syntax error: expected '->' after tuple type.\n"
  | 128 -> "Syntax error: expected ']' to close bracketed type.\n"
  | 126 ->
      "Syntax error: expected a type expression after '->' in type annotation.\n"
  | 125 ->
      "Syntax error: unexpected token after type; expected '->', ',', ')', or \
       ']'.\n"
  | 120 ->
      "Syntax error: expected a type expression after '[' in type annotation.\n"
  | 119 ->
      "Syntax error: expected a type expression after '(' in type annotation.\n"
  | 116 ->
      "Syntax error: expected a type expression after ':' in 'let' binding.\n"
  | 114 ->
      "Syntax error: unexpected token after '=' comparison; expected an \
       operator or end of expression.\n"
  | 113 -> "Syntax error: expected an expression after '='.\n"
  | 112 ->
      "Syntax error: unexpected token after '>' comparison; expected an \
       operator or end of expression.\n"
  | 111 -> "Syntax error: expected an expression after '>'.\n"
  | 110 ->
      "Syntax error: unexpected token after '<' comparison; expected an \
       operator or end of expression.\n"
  | 109 -> "Syntax error: expected an expression after '<'.\n"
  | 108 ->
      "Syntax error: unexpected token after '!=' comparison; expected an \
       operator or end of expression.\n"
  | 104 -> "Syntax error: expected an expression after '::'.\n"
  | 103 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 102 -> "Syntax error: expected an expression after '-'.\n"
  | 101 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 100 -> "Syntax error: expected an expression after '+'.\n"
  | 97 -> "Syntax error: expected an argument after ',' in function call.\n"
  | 96 -> "Syntax error: expected ',' or ')' after argument in function call.\n"
  | 94 -> "Syntax error: expected an argument or ')' in function call.\n"
  | 93 ->
      "Syntax error: unexpected token after expression; expected an operator, \
       '(', or end of expression.\n"
  | 92 -> "Syntax error: expected an expression after '!='.\n"
  | 88 ->
      "Syntax error: expected an expression after 'else' in 'if' expression.\n"
  | 87 ->
      "Syntax error: expected 'else' or end of expression after 'then' branch.\n"
  | 86 ->
      "Syntax error: expected an expression after 'then' in 'if' expression.\n"
  | 85 -> "Syntax error: expected 'then' after condition in 'if' expression.\n"
  | 80 -> "Syntax error: expected an expression after '*'.\n"
  | 77 -> "Syntax error: expected an expression after '/'.\n"
  | 76 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 70 -> "Syntax error: expected a condition expression after 'if'.\n"
  | 69 -> "Syntax error: expected an expression after '=' in 'let' binding.\n"
  | 66 -> "Syntax error: expected '{' to begin function body.\n"
  | 65 ->
      "Syntax error: expected a return type after '->' in function definition.\n"
  | 64 ->
      "Syntax error: expected '{' or '->' after parameter list in function \
       definition.\n"
  | 60 -> "Syntax error: expected a parameter after ',' in parameter list.\n"
  | 59 -> "Syntax error: expected ')', ',', or end of parameter list.\n"
  | 55 -> "Syntax error: expected an identifier after ',' in identifier list.\n"
  | 54 -> "Syntax error: expected ',' or ']' in identifier list.\n"
  | 53 -> "Syntax error: expected an identifier in grouped parameter list.\n"
  | 52 ->
      "Syntax error: expected '[' after parameter type for grouped parameters, \
       or ',' or ')'.\n"
  | 50 -> "Syntax error: expected a type after ':' in parameter annotation.\n"
  | 49 ->
      "Syntax error: expected ':', ')', or ',' after identifier in parameter.\n"
  | 47 -> "Syntax error: expected ')' to close parenthesized parameter type.\n"
  | 45 -> "Syntax error: expected ']' to close bracketed parameter type.\n"
  | 40 -> "Syntax error: expected a type after '[' in parameter type.\n"
  | 39 ->
      "Syntax error: expected a parameter type after '(' in type annotation.\n"
  | 36 -> "Syntax error: expected '{' to begin function body.\n"
  | 35 -> "Syntax error: expected ',' or ')' in return type list.\n"
  | 33 ->
      "Syntax error: expected a return type after ',' in return type list.\n"
  | 31 -> "Syntax error: expected ')', ',', or '->' in return type list.\n"
  | 29 ->
      "Syntax error: expected a return type after '->' in tuple return type.\n"
  | 28 -> "Syntax error: expected '->' after tuple return type.\n"
  | 25 -> "Syntax error: expected ']' to close bracketed return type.\n"
  | 23 ->
      "Syntax error: expected a return type after '->' in return type \
       annotation.\n"
  | 22 ->
      "Syntax error: unexpected token after return type; expected '->', ',', \
       or '{'.\n"
  | 17 -> "Syntax error: expected a type after '[' in return type annotation.\n"
  | 16 ->
      "Syntax error: expected a return type after '(' in return type annotation.\n"
  | 13 ->
      "Syntax error: expected a return type after '->' in function definition.\n"
  | 12 ->
      "Syntax error: expected '{' or '->' after '()' in function definition.\n"
  | 9 ->
      "Syntax error: expected a parameter or ')' in function parameter list.\n"
  | 8 ->
      "Syntax error: expected '=', '{', '(', or '->' after identifier in 'let' \
       binding.\n"
  | 7 -> "Syntax error: expected an identifier after 'let'.\n"
  | 6 -> "Syntax error: expected an expression to match on after 'match'.\n"
  | 5 -> "Syntax error: expected an expression or '}' after '{'.\n"
  | 3 -> "Syntax error: expected an expression or ')' after '('.\n"
  | 0 ->
      "Syntax error: unexpected token at start of program; expected an \
       expression.\n"
  | _ -> raise Not_found
