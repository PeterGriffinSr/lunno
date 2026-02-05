%{
    open Ast

    let merge_span e1 e2 =
        let span_of_expr = function
            | IntLiteral (_, sp) 
            | FloatLiteral (_, sp) 
            | Variable (_, sp) -> sp
            | Binary { span; _ } -> span
            | _ -> failwith "merge_span: unsupported expr"
        in
        let sp1 = span_of_expr e1 in
        let sp2 = span_of_expr e2 in
        (fst sp1, snd sp2)
%}

%token <int64 * Lunno_common.Span.t> Integer
%token <float * Lunno_common.Span.t> FloatingPoint
%token <string * Lunno_common.Span.t> Identifier
%token <string * Lunno_common.Span.t> String

%token <Lunno_common.Span.t> Let Function If Then Else Match In
%token <Lunno_common.Span.t> IntegerType FloatingPointType StringType BooleanType UnitType
%token <Lunno_common.Span.t> LeftParen RightParen LeftBrace RightBrace LeftBracket RightBracket
%token <Lunno_common.Span.t> Plus Minus Asterisk Slash Equal NotEqual Less Greater
%token <Lunno_common.Span.t> Comma Colon Pipe Cons Arrow
%token <Lunno_common.Span.t> EndOfFile

%start program
%type <Ast.program> program

%%

program:
    | expr_list EndOfFile { $1 }

expr_list:
    | expr expr_list { $1 :: $2 }
    | expr { [$1] }

expr:
    | additive_expr { $1 }

additive_expr:
    | additive_expr Plus multiplicative_expr { Binary { op = OpAdd; left = $1; right = $3; span = merge_span $1 $3 } }
    | additive_expr Minus multiplicative_expr { Binary { op = OpSub; left = $1; right = $3; span = merge_span $1 $3 } }
    | multiplicative_expr { $1 }

multiplicative_expr:
    | multiplicative_expr Asterisk primary_expr { Binary { op = OpMul; left = $1; right = $3; span = merge_span $1 $3 } }
    | multiplicative_expr Slash primary_expr { Binary { op = OpDiv; left = $1; right = $3; span = merge_span $1 $3 } }
    | primary_expr { $1 }

primary_expr:
    | Integer { let (i, span) = $1 in IntLiteral (i, span) }
    | FloatingPoint { let (f, span) = $1 in FloatLiteral (f, span) }
    | Identifier { let (name, span) = $1 in Variable (name, span) }
    | LeftParen expr RightParen { $2 }