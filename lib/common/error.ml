open Lexing

type error_code =
  | E_Lex_UnexpectedChar
  | E_Lex_InvalidInt
  | E_Lex_InvalidFloat
  | E_Lex_UnterminatedString
  | E_Lex_InvalidEscape
  | E_Lex_NewlineInString
  | E_Lex_EmptyString
  | E_Parse_UnexpectedToken
  | E_Type_AlreadyDefined
  | E_Type_UndefinedVariable
  | E_Type_TypeMismatch
  | E_Type_ArityMismatch
  | E_Type_MissingAnnotation
  | E_Type_IfBranchMismatch
  | E_Type_MissingElseBranch
  | E_Type_NotAFunction
  | E_Type_MatchBranchMismatch
  | E_Type_PatternTypeMismatch

exception LexerError of { code : error_code; msg : string; span : Span.t }
exception TypeError of { code : error_code; msg : string; span : Span.t }
exception ParseError of { code : error_code; msg : string; span : Span.t }

let string_of_code = function
  | E_Lex_UnexpectedChar -> "E1001"
  | E_Lex_InvalidInt -> "E1002"
  | E_Lex_InvalidFloat -> "E1003"
  | E_Lex_UnterminatedString -> "E1004"
  | E_Lex_InvalidEscape -> "E1005"
  | E_Lex_NewlineInString -> "E1006"
  | E_Lex_EmptyString -> "E1007"
  | E_Parse_UnexpectedToken -> "E2001"
  | E_Type_AlreadyDefined -> "E3001"
  | E_Type_UndefinedVariable -> "E3002"
  | E_Type_TypeMismatch -> "E3003"
  | E_Type_ArityMismatch -> "E3004"
  | E_Type_MissingAnnotation -> "E3005"
  | E_Type_IfBranchMismatch -> "E3006"
  | E_Type_MissingElseBranch -> "E3007"
  | E_Type_NotAFunction -> "E3008"
  | E_Type_MatchBranchMismatch -> "E3009"
  | E_Type_PatternTypeMismatch -> "E3010"

let print_error (lines : string array) = function
  | LexerError { code; msg; span = start_pos, _ }
  | ParseError { code; msg; span = start_pos, _ }
  | TypeError { code; msg; span = start_pos, _ } ->
      let line_num = start_pos.pos_lnum in
      let col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
      let line =
        if line_num > 0 && line_num <= Array.length lines then
          lines.(line_num - 1)
        else ""
      in
      let gutter = String.length (string_of_int line_num) in
      let pad = String.make gutter ' ' in
      Printf.eprintf "Error[%s]: %s\n" (string_of_code code) msg;
      Printf.eprintf " %s--> line %d, column %d\n" pad line_num col;
      Printf.eprintf "  %d | %s\n" line_num line;
      Printf.eprintf "  %s | %s^\n" pad (String.make (col - 1) ' ');
      flush stderr
  | _ -> ()

let already_defined name span =
  TypeError
    {
      code = E_Type_AlreadyDefined;
      msg = Printf.sprintf "name '%s' is already defined in this scope" name;
      span;
    }

let undefined_variable name span =
  TypeError
    {
      code = E_Type_UndefinedVariable;
      msg = Printf.sprintf "undefined variable '%s'" name;
      span;
    }

let type_mismatch expected got span =
  TypeError
    {
      code = E_Type_TypeMismatch;
      msg = Printf.sprintf "type mismatch: expected '%s', got '%s'" expected got;
      span;
    }

let arity_mismatch expected got span =
  TypeError
    {
      code = E_Type_ArityMismatch;
      msg =
        Printf.sprintf "arity mismatch: expected %d argument(s), got %d"
          expected got;
      span;
    }

let missing_annotation name span =
  TypeError
    {
      code = E_Type_MissingAnnotation;
      msg = Printf.sprintf "missing type annotation for '%s'" name;
      span;
    }

let if_branch_mismatch then_ty else_ty span =
  TypeError
    {
      code = E_Type_IfBranchMismatch;
      msg =
        Printf.sprintf
          "if branches have different types: then is '%s', else is '%s'" then_ty
          else_ty;
      span;
    }

let missing_else_branch span =
  TypeError
    {
      code = E_Type_MissingElseBranch;
      msg = "if expression is missing an else branch";
      span;
    }

let not_a_function ty span =
  TypeError
    {
      code = E_Type_NotAFunction;
      msg = Printf.sprintf "expected a function, got '%s'" ty;
      span;
    }

let match_branch_mismatch expected got span =
  TypeError
    {
      code = E_Type_MatchBranchMismatch;
      msg =
        Printf.sprintf "match branch type mismatch: expected '%s', got '%s'"
          expected got;
      span;
    }

let pattern_type_mismatch expected got span =
  TypeError
    {
      code = E_Type_PatternTypeMismatch;
      msg =
        Printf.sprintf "pattern type mismatch: expected '%s', got '%s'" expected
          got;
      span;
    }
