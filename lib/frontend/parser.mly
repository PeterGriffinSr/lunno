%{
    open Ast

    let rec contains_variable name expr =
        match expr with
        | Literal _ -> false
        | Variable (v, _) -> v = name
        | Lambda { body; _ } -> contains_variable name body
        | Apply (f, args, _) ->
            contains_variable name f || List.exists (contains_variable name) args
        | Let { body; _ } -> contains_variable name body
        | If { cond; then_; else_; _ } ->
            contains_variable name cond || 
            contains_variable name then_ || 
            (match else_ with
            | Some e -> contains_variable name e
            | None -> false)
        | Match { scrutinee; cases; _ } ->
            contains_variable name scrutinee ||
            List.exists (fun { body; guard; _ } ->
                contains_variable name body ||
                (match guard with Some g -> contains_variable name g | None -> false)
            ) cases
        | Block (exprs, _) -> List.exists (contains_variable name) exprs
        | Binary { left; right; _ } ->
            contains_variable name left || contains_variable name right
        | Unary { expr; _ } -> contains_variable name expr

    let merge e1 e2 =
        let span_of = function
            | `Expr e ->
                begin match e with
                    | Literal (_, sp) -> sp
                    | Variable (_, sp) -> sp
                    | Binary { span; _ } -> span
                    | Let { span; _ } -> span
                    | Lambda { span; _ } -> span
                    | Block (_, span) -> span
                    | Apply (_, _, span) -> span
                    | If { span; _ } -> span
                    | Match { span; _ } -> span
                    | Unary { span; _ } -> span
                end
            | `Span sp -> sp
            | `Pattern p ->
                begin match p with
                    | PWildcard sp -> sp
                    | PVariable (_, sp) -> sp
                    | PIntLiteral (_, sp) -> sp
                    | PFloatLiteral (_, sp) -> sp
                    | PStringLiteral (_, sp) -> sp
                    | PBooleanLiteral (_, sp) -> sp
                    | PNil sp -> sp
                    | PCons (_, _, sp) -> sp
                end
        in
        let sp1 = span_of e1 in
        let sp2 = span_of e2 in
        (fst sp1, snd sp2)
%}

%token <int64 * Lunno_common.Span.t> Integer
%token <float * Lunno_common.Span.t> FloatingPoint
%token <string * Lunno_common.Span.t> Identifier
%token <string * Lunno_common.Span.t> String
%token <bool * Lunno_common.Span.t> Boolean
%token <unit * Lunno_common.Span.t> Unit

%token <Lunno_common.Span.t> Let If Then Else Match
%token <Lunno_common.Span.t> IntegerType FloatingPointType StringType BooleanType UnitType
%token <Lunno_common.Span.t> LeftParen RightParen LeftBrace RightBrace LeftBracket RightBracket
%token <Lunno_common.Span.t> Plus Minus Asterisk Slash Equal NotEqual Less Greater
%token <Lunno_common.Span.t> Comma Colon Pipe Cons Arrow Underscore
%token <Lunno_common.Span.t> EndOfFile

%nonassoc THEN
%nonassoc Else

%start program
%type <Ast.program> program
%type <Ast.ty> type_expr type_primary
%type <Ast.ty list> type_expr_list
%type <Ast.pattern> pattern cons_pattern primary_pattern
%type <Ast.match_case> match_case
%type <Ast.match_case list> match_cases

%%

program:
    | expr_list EndOfFile { $1 }

expr_list:
    | expr expr_list { $1 :: $2 }
    | expr { [$1] }

expr:
    | let_expr { $1 }
    | if_expr { $1 }
    | match_expr { $1 }
    | comparison_expr { $1 }

comparison_expr:
    | comparison_expr Equal additive_expr { Binary { op = OpEqual; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr NotEqual additive_expr { Binary { op = OpNotEqual; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr Less additive_expr { Binary { op = OpLess; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr Greater additive_expr { Binary { op = OpGreater; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | additive_expr { $1 }

additive_expr:
    | additive_expr Plus multiplicative_expr { Binary { op = OpAdd; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | additive_expr Minus multiplicative_expr { Binary { op = OpSub; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | multiplicative_expr { $1 }

multiplicative_expr:
    | multiplicative_expr Asterisk primary_expr { Binary { op = OpMul; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | multiplicative_expr Slash primary_expr { Binary { op = OpDiv; left = $1; right = $3; span = merge (`Expr $1) (`Expr $3) } }
    | call_expr { $1 }

call_expr:
    | call_expr LeftParen RightParen { Apply ($1, [], merge (`Expr $1) (`Span $3)) }
    | call_expr LeftParen arg_list RightParen { Apply ($1, $3, merge (`Expr $1) (`Span $4)) }
    | primary_expr { $1 }

primary_expr:
    | Integer { let (i, span) = $1 in Literal (LInt i, span) }
    | FloatingPoint { let (f, span) = $1 in Literal (LFloat f, span) }
    | String { let (s, span) = $1 in Literal (LString s, span) }
    | Boolean { let (b, span) = $1 in Literal (LBool b, span) }
    | Unit { let ((), span) = $1 in Literal (LUnit, span) }
    | Identifier { let (name, span) = $1 in Variable (name, span) }
    | LeftParen expr RightParen { $2 }
    | block_expr { $1 }

if_expr:
    | If expr Then expr Else expr { If { cond = $2; then_ = $4; else_ = Some $6; span = merge (`Span $1) (`Expr $6) } }
    | If expr Then expr %prec THEN { If { cond = $2; then_ = $4; else_ = None; span = merge (`Span $1) (`Expr $4) } }

block_expr:
    | LeftBrace expr_list RightBrace { Block ($2, merge (`Span $1) (`Span $3)) }

match_expr:
    | Match expr LeftBrace match_cases RightBrace { Match { scrutinee = $2; cases = $4; span = merge (`Span $1) (`Span $5) } }

match_cases:
    | Pipe match_case { [$2] }
    | Pipe match_case match_cases { $2 :: $3 }

match_case:
    | pattern Arrow expr { { pattern = $1; guard = None; body = $3; span = merge (`Pattern $1) (`Expr $3) } }
    | pattern If expr Arrow expr { { pattern = $1; guard = Some $3; body = $5; span = merge (`Pattern $1) (`Expr $5) } }

pattern:
    | cons_pattern { $1 }

cons_pattern:
    | primary_pattern Cons cons_pattern { PCons ($1, $3, merge (`Pattern $1) (`Pattern $3)) }
    | primary_pattern { $1 }

primary_pattern:
    | Underscore { PWildcard $1 }
    | Identifier { let (name, span) = $1 in PVariable (name, span) }
    | Integer { let (i, span) = $1 in PIntLiteral (i, span) }
    | FloatingPoint { let (f, span) = $1 in PFloatLiteral (f, span) }
    | String { let (s, span) = $1 in PStringLiteral (s, span) }
    | Boolean { let (b, span) = $1 in PBooleanLiteral (b, span) }
    | LeftBracket RightBracket { PNil (merge (`Span $1) (`Span $2)) }
    | LeftParen pattern RightParen { $2 }

let_expr:
    | Let Identifier Equal expr { let (name, _) = $2 in Let { name; ty = None; body = $4; span = merge (`Span $1) (`Expr $4) } }
    | Let Identifier Colon type_expr Equal expr { let (name, _) = $2 in Let { name; ty = Some $4; body = $6; span = merge (`Span $1) (`Expr $6) } }
    | Let Identifier LeftParen RightParen block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $5 in
        let lambda = Lambda {
            params = [];
            ret_ty = None;
            body = $5;
            is_recursive;
            span = merge (`Span $3) (`Expr $5)
        } in
        Let { name; ty = None; body = lambda; span = merge (`Span $1) (`Expr $5) }
    }
    | Let Identifier LeftParen RightParen Arrow type_expr block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $7 in
        let lambda = Lambda {
            params = [];
            ret_ty = Some $6;
            body = $7;
            is_recursive;
            span = merge (`Span $3) (`Expr $7)
        } in
        Let { name; ty = None; body = lambda; span = merge (`Span $1) (`Expr $7) }
    }
    | Let Identifier LeftParen param_list RightParen block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $6 in
        let lambda = Lambda {
            params = $4;
            ret_ty = None;
            body = $6;
            is_recursive;
            span = merge (`Span $3) (`Expr $6)
        } in
        Let { name; ty = None; body = lambda; span = merge (`Span $3) (`Expr $6) }
    }
    | Let Identifier LeftParen param_list RightParen Arrow type_expr block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $8 in
        let lambda = Lambda { 
            params = $4; 
            ret_ty = Some $7; 
            body = $8;
            is_recursive;
            span = merge (`Span $3) (`Expr $8) 
        } in
        Let { name; ty = None; body = lambda; span = merge (`Span $1) (`Expr $8) }
    }

type_expr:
    | type_primary Arrow type_expr { TyFunction ([$1], $3) }
    | LeftParen type_expr_list RightParen Arrow type_expr { TyFunction ($2, $5) }
    | type_primary { $1 }

type_primary:
    | IntegerType { TyInt }
    | FloatingPointType { TyFloat }
    | StringType { TyString }
    | BooleanType { TyBool }
    | UnitType { TyUnit }
    | Identifier { let (name, _) = $1 in TyVar name }
    | LeftBracket type_expr RightBracket { TyList $2 }
    | LeftParen type_expr RightParen { $2 }

type_expr_list:
    | type_expr Comma type_expr { [$1; $3] }
    | type_expr Comma type_expr_list { $1 :: $3 }

param_list:
    | param_or_group { $1 }
    | param_or_group Comma param_list { $1 @ $3 }

param_or_group:
    | param { [$1] }
    | type_primary LeftBracket identifier_list RightBracket { List.map (fun (id, sp) -> { name = id; ty = Some $1; span = sp }) $3 }

param:
    | Identifier { let (name, span) = $1 in { name; ty = None; span } }
    | Identifier Colon type_expr { let (name, span) = $1 in { name; ty = Some $3; span } }

identifier_list:
    | Identifier { [$1] }
    | Identifier Comma identifier_list { $1 :: $3 }

arg_list:
    | expr { [$1] }
    | expr Comma arg_list { $1 :: $3 }