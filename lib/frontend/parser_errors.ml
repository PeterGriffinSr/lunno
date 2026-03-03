(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
 fun s ->
  match s with
  | 260 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 258 ->
      "Syntax error: unexpected token after import; expected another 'import' \
       or an expression.\n"
  | 256 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 254 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 247 ->
      "Syntax error: unexpected token after type declaration; expected another \
       'data' declaration or an expression.\n"
  | 245 ->
      "Syntax error: unexpected token at end of program; expected end of file.\n"
  | 238 ->
      "Syntax error: expected '|' or '}' after variant in 'data' declaration.\n"
  | 236 ->
      "Syntax error: expected a type expression after ',' in variant field list.\n"
  | 235 -> "Syntax error: expected ',' or ')' after variant field type.\n"
  | 232 ->
      "Syntax error: expected a type expression for variant field after '('.\n"
  | 231 ->
      "Syntax error: expected '|', '}', or '(' after variant name in 'data' \
       declaration.\n"
  | 230 ->
      "Syntax error: expected a variant name after '|' in 'data' declaration.\n"
  | 229 ->
      "Syntax error: expected '|' to begin variant list in 'data' declaration.\n"
  | 228 -> "Syntax error: expected '{' after '=' in 'data' declaration.\n"
  | 227 -> "Syntax error: expected '=' after type name in 'data' declaration.\n"
  | 226 ->
      "Syntax error: expected an identifier after 'data' in type declaration.\n"
  | 224 ->
      "Syntax error: expected a string literal (module path) after 'import'.\n"
  | 222 -> "Syntax error: expected ')' to close parenthesized expression.\n"
  | 220 -> "Syntax error: expected ']' to close list expression.\n"
  | 218 -> "Syntax error: expected ']' to close range expression.\n"
  | 217 ->
      "Syntax error: expected an expression after '..' in range expression.\n"
  | 216 ->
      "Syntax error: expected ',', '..', or ']' after expression in list.\n"
  | 214 ->
      "Syntax error: unexpected token after expression; expected another \
       expression or end of block.\n"
  | 212 -> "Syntax error: expected '}' to close block expression.\n"
  | 208 ->
      "Syntax error: unexpected token after match case; expected '|' or '}'.\n"
  | 206 -> "Syntax error: expected an expression after '->' in match case.\n"
  | 204 -> "Syntax error: expected an expression after '->' in match case.\n"
  | 203 -> "Syntax error: expected '->' after guard expression in match case.\n"
  | 202 ->
      "Syntax error: expected a guard expression after 'if' in match case.\n"
  | 201 -> "Syntax error: expected '->' or 'if' after pattern in match case.\n"
  | 199 -> "Syntax error: expected ')' to close parenthesized pattern.\n"
  | 196 -> "Syntax error: expected a pattern after ',' in pattern list.\n"
  | 195 ->
      "Syntax error: expected ',' or ')' after pattern in constructor argument \
       list.\n"
  | 191 -> "Syntax error: expected a pattern after '::' in cons pattern.\n"
  | 190 ->
      "Syntax error: unexpected token in match pattern; expected '::', '->', \
       or 'if'.\n"
  | 186 ->
      "Syntax error: expected a pattern or ')' after '(' in constructor pattern.\n"
  | 185 ->
      "Syntax error: unexpected token in match pattern; expected '::', '->', \
       or 'if'.\n"
  | 182 ->
      "Syntax error: expected ']' for empty list pattern or a pattern element \
       after '['.\n"
  | 181 -> "Syntax error: expected a pattern after '(' in match case.\n"
  | 178 -> "Syntax error: expected a pattern after '|' in match case.\n"
  | 177 -> "Syntax error: expected '|' to begin match cases.\n"
  | 176 -> "Syntax error: expected '{' after match expression.\n"
  | 173 -> "Syntax error: expected '{' to begin function body.\n"
  | 172 ->
      "Syntax error: expected a return type after '->' in function definition.\n"
  | 170 -> "Syntax error: expected an expression after '=' in 'let' binding.\n"
  | 169 ->
      "Syntax error: expected '=' after type annotation in 'let' binding.\n"
  | 168 -> "Syntax error: expected ',' or ')' in type list.\n"
  | 166 -> "Syntax error: expected a type expression after ',' in type list.\n"
  | 164 -> "Syntax error: expected ')', ',', or '->' in type list.\n"
  | 162 ->
      "Syntax error: expected a type expression after '->' in tuple type.\n"
  | 161 -> "Syntax error: expected '->' after tuple type.\n"
  | 158 ->
      "Syntax error: expected a type expression after '->' in type annotation.\n"
  | 156 ->
      "Syntax error: unexpected token after type; expected '->', ',', ')', or \
       ']'.\n"
  | 145 ->
      "Syntax error: expected a type expression after '(' in type annotation.\n"
  | 141 ->
      "Syntax error: expected a type expression after ':' in 'let' binding.\n"
  | 139 ->
      "Syntax error: unexpected token after '=' comparison; expected an \
       operator or end of expression.\n"
  | 138 -> "Syntax error: expected an expression after '='.\n"
  | 137 ->
      "Syntax error: unexpected token after '>' comparison; expected an \
       operator or end of expression.\n"
  | 136 -> "Syntax error: expected an expression after '>'.\n"
  | 135 ->
      "Syntax error: unexpected token after '<' comparison; expected an \
       operator or end of expression.\n"
  | 134 -> "Syntax error: expected an expression after '<'.\n"
  | 133 ->
      "Syntax error: unexpected token after '<>' comparison; expected an \
       operator or end of expression.\n"
  | 131 ->
      "Syntax error: expected ')' to close argument list in function call.\n"
  | 129 -> "Syntax error: expected an expression after '::'.\n"
  | 128 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 127 -> "Syntax error: expected an expression after '-'.\n"
  | 126 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 125 -> "Syntax error: expected an expression after '+'.\n"
  | 122 -> "Syntax error: expected an argument after ',' in function call.\n"
  | 121 ->
      "Syntax error: expected ',' or ')' after argument in function call.\n"
  | 119 -> "Syntax error: expected an argument or ')' in function call.\n"
  | 118 ->
      "Syntax error: unexpected token after expression; expected an operator, \
       '(', or end of expression.\n"
  | 117 -> "Syntax error: expected an expression after '<>'.\n"
  | 113 ->
      "Syntax error: expected an expression after 'else' in 'if' expression.\n"
  | 112 ->
      "Syntax error: expected 'else' or end of expression after 'then' branch.\n"
  | 111 ->
      "Syntax error: expected an expression after 'then' in 'if' expression.\n"
  | 110 -> "Syntax error: expected 'then' after condition in 'if' expression.\n"
  | 105 -> "Syntax error: expected an identifier after '.' in member access.\n"
  | 102 -> "Syntax error: expected an expression after '*'.\n"
  | 99 -> "Syntax error: expected an expression after '/'.\n"
  | 98 ->
      "Syntax error: unexpected token after expression; expected an operator \
       or end of expression.\n"
  | 92 -> "Syntax error: expected a condition expression after 'if'.\n"
  | 91 -> "Syntax error: expected an expression after '=' in 'let' binding.\n"
  | 88 -> "Syntax error: expected '{' to begin function body.\n"
  | 87 ->
      "Syntax error: expected a return type after '->' in function definition.\n"
  | 86 ->
      "Syntax error: expected '{' or '->' after parameter list in function \
       definition.\n"
  | 82 -> "Syntax error: expected a parameter after ',' in parameter list.\n"
  | 81 ->
      "Syntax error: expected ')' or ',' after parameter in parameter list.\n"
  | 77 -> "Syntax error: expected an identifier after ',' in identifier list.\n"
  | 76 -> "Syntax error: expected ',' or ']' in identifier list.\n"
  | 75 -> "Syntax error: expected an identifier in grouped parameter list.\n"
  | 74 ->
      "Syntax error: expected '[' after parameter type for grouped parameters, \
       or ',' or ')'.\n"
  | 72 -> "Syntax error: expected a type after ':' in parameter annotation.\n"
  | 71 ->
      "Syntax error: expected ':', ')', or ',' after identifier in parameter.\n"
  | 70 -> "Syntax error: expected ',' or ')' in type list.\n"
  | 68 -> "Syntax error: expected a type expression after ',' in type list.\n"
  | 66 -> "Syntax error: expected ')', ',', or '->' in type list.\n"
  | 64 -> "Syntax error: expected a type expression after '->' in tuple type.\n"
  | 63 -> "Syntax error: expected '->' after tuple type.\n"
  | 60 ->
      "Syntax error: expected a type expression after '->' in type annotation.\n"
  | 58 ->
      "Syntax error: unexpected token after type; expected '->', ',', ')', or \
       ']'.\n"
  | 47 ->
      "Syntax error: expected a parameter type after '(' in type annotation.\n"
  | 43 -> "Syntax error: expected '{' to begin function body.\n"
  | 42 -> "Syntax error: expected ',' or ')' in return type list.\n"
  | 40 ->
      "Syntax error: expected a return type after ',' in return type list.\n"
  | 38 -> "Syntax error: expected ')', ',', or '->' in return type list.\n"
  | 36 ->
      "Syntax error: expected a return type after '->' in tuple return type.\n"
  | 35 -> "Syntax error: expected '->' after tuple return type.\n"
  | 32 ->
      "Syntax error: expected a return type after '->' in return type \
       annotation.\n"
  | 30 ->
      "Syntax error: unexpected token after return type; expected '->', ',', \
       or '{'.\n"
  | 19 ->
      "Syntax error: expected a return type after '(' in return type annotation.\n"
  | 15 ->
      "Syntax error: expected a return type after '->' in function definition.\n"
  | 14 ->
      "Syntax error: expected '{' or '->' after '()' in function definition.\n"
  | 11 ->
      "Syntax error: expected a parameter or ')' in function parameter list.\n"
  | 10 ->
      "Syntax error: expected '=', '{', '(', or '->' after identifier in 'let' \
       binding.\n"
  | 9 -> "Syntax error: expected an identifier after 'let'.\n"
  | 8 -> "Syntax error: expected an expression to match on after 'match'.\n"
  | 7 -> "Syntax error: expected an expression or '}' after '{'.\n"
  | 5 -> "Syntax error: expected an expression or ']' after '['.\n"
  | 3 -> "Syntax error: expected an expression or ')' after '('.\n"
  | 0 ->
      "Syntax error: unexpected token at start of program; expected an \
       expression.\n"
  | _ -> raise Not_found
