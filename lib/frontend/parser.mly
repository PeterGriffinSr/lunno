%{
    open Lunno_common.Ast

    let rec contains_variable name expr =
        match expr with
        | Literal _ -> false
        | Variable (v, _) -> v = name
        | Lambda { lambda_body; _ } -> contains_variable name lambda_body
        | Apply (f, args, _) ->
            contains_variable name f || List.exists (contains_variable name) args
        | Let { let_body; _ } -> contains_variable name let_body
        | If { cond; then_; else_; _ } ->
            contains_variable name cond || 
            contains_variable name then_ || 
            (match else_ with
            | Some e -> contains_variable name e
            | None -> false)
        | Match { scrutinee; cases; _ } ->
            contains_variable name scrutinee ||
            List.exists (fun { case_body; guard; _ } ->
                contains_variable name case_body ||
                (match guard with Some g -> contains_variable name g | None -> false)
            ) cases
        | Block (exprs, _) -> List.exists (contains_variable name) exprs
        | Binary { left; right; _ } ->
            contains_variable name left || contains_variable name right
        | Unary { expr; _ } -> contains_variable name expr
        | MemberAccess (obj, _, _) -> contains_variable name obj
        | Range (e1, e2, _) -> contains_variable name e1 || contains_variable name e2
        | Constructor (_, args, _) -> List.exists (contains_variable name) args

    let merge e1 e2 =
        let span_of = function
            | `Expr e ->
                begin match e with
                    | Literal (_, sp) -> sp
                    | Variable (_, sp) -> sp
                    | Binary { binary_span; _ } -> binary_span
                    | Let { let_span; _ } -> let_span
                    | Lambda { lambda_span; _ } -> lambda_span
                    | Block (_, span) -> span
                    | Apply (_, _, span) -> span
                    | If { if_span; _ } -> if_span
                    | Match { match_span; _ } -> match_span
                    | Unary { unary_span; _ } -> unary_span
                    | MemberAccess (_, _, span) -> span
                    | Range (_, _, span) -> span
                    | Constructor (_, _, span) -> span
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
                    | PConstructor (_, _, sp) -> sp
                end
        in
        let sp1 = span_of e1 in
        let sp2 = span_of e2 in
        (fst sp1, snd sp2)

    let desugar_list close_span elems =
        List.fold_right (fun elem acc -> 
        Binary { binary_op = OpCons; left = elem; right = acc; binary_span = merge (`Expr elem) (`Expr acc) })
    elems
    (Literal (LNil, close_span))
%}

%token <int64 * Lunno_common.Span.t> Integer
%token <float * Lunno_common.Span.t> FloatingPoint
%token <string * Lunno_common.Span.t> Identifier
%token <string * Lunno_common.Span.t> String
%token <bool * Lunno_common.Span.t> Boolean
%token <unit * Lunno_common.Span.t> Unit
%token <char * Lunno_common.Span.t> Quote

%token <Lunno_common.Span.t> KwLet KwIf KwThen KwElse KwMatch KwImport KwData
%token <Lunno_common.Span.t> IntegerType I8Type I16Type I32Type I64Type FloatingPointType F32Type F64Type StringType BooleanType UnitType ListType
%token <Lunno_common.Span.t> LeftParen RightParen LeftBrace RightBrace LeftBracket RightBracket
%token <Lunno_common.Span.t> Plus Minus Asterisk Slash Equal NotEqual Less Greater
%token <Lunno_common.Span.t> Comma Colon Pipe Cons Arrow Underscore Dot DotDot
%token <Lunno_common.Span.t> EndOfFile

%nonassoc THEN
%nonassoc KwElse
%nonassoc BelowApp
%left LeftParen

%start program
%type <Lunno_common.Ast.program> program
%type <Lunno_common.Ast.expr> expr primary_expr additive_expr multiplicative_expr comparison_expr cons_expr call_expr block_expr if_expr let_expr match_expr member_expr
%type <Lunno_common.Ast.expr list> expr_list arg_list
%type <Lunno_common.Ast.pattern> pattern primary_pattern cons_pattern
%type <Lunno_common.Ast.pattern list> pattern_list
%type <Lunno_common.Ast.match_case> match_case
%type <Lunno_common.Ast.match_case list> match_cases
%type <Lunno_common.Ast.import> import
%type <Lunno_common.Ast.import list> import_list
%type <Lunno_common.Ast.ty> type_expr type_primary ret_type ret_type_primary param_type param_type_primary
%type <Lunno_common.Ast.ty list> type_expr_list ret_type_list param_type_list variant_field_list
%type <Lunno_common.Ast.param> param
%type <Lunno_common.Ast.param list> param_list param_or_group
%type <Lunno_common.Ast.variant> variant
%type <Lunno_common.Ast.variant list> variant_list
%type <(string * Lunno_common.Span.t) list> identifier_list
%type <Lunno_common.Ast.type_decl> type_decl
%type <Lunno_common.Ast.type_decl list> type_decl_list

%%

program:
    | import_list type_decl_list expr_list EndOfFile { { imports = $1; type_decls = $2; body = $3 } }
    | import_list type_decl_list EndOfFile { { imports = $1; type_decls = $2; body = [] } }
    | import_list expr_list EndOfFile { { imports = $1; type_decls = []; body = $2 } }
    | import_list EndOfFile { { imports = $1; type_decls = []; body = [] } }
    | type_decl_list expr_list EndOfFile { { imports = []; type_decls = $1; body = $2 } }
    | type_decl_list EndOfFile { { imports = []; type_decls = $1; body = [] } }
    | expr_list EndOfFile { { imports = []; type_decls = []; body = $1 } }
    | EndOfFile { { imports = []; type_decls = []; body = [] } }

import_list:
    | import import_list { $1 :: $2 }
    | import { [$1] }

import:
    | KwImport String {
        let (path, span) = $2 in
        match String.split_on_char ':' path with
        | [module_; item] ->
            { module_; item; import_span = merge (`Span $1) (`Span span) }
        | [] | [_] | _ :: _ :: _ :: _ -> assert false
    }

type_decl_list:
    | type_decl type_decl_list { $1 :: $2 }
    | type_decl { [$1] }

type_decl:
    | KwData Identifier Equal LeftBrace variant_list RightBrace {
        let (name, _) = $2 in { type_name = name; variants = $5; type_span = merge (`Span $1) (`Span $6) }
    }

variant_list:
    | Pipe variant { [$2] }
    | Pipe variant variant_list { $2 :: $3 }

variant:
    | Identifier { let (name, span) = $1 in { variant_name = name; variant_fields = []; variant_span = span } }
    | Identifier LeftParen variant_field_list RightParen { 
        let (name, span) = $1 in { variant_name = name; variant_fields = $3; variant_span = merge (`Span span) (`Span $4) } 
    }

variant_field_list:
    | type_expr { [$1] }
    | type_expr Comma variant_field_list { $1 :: $3 }

expr_list:
    | expr expr_list { $1 :: $2 }
    | expr { [$1] }

expr:
    | let_expr { $1 }
    | if_expr { $1 }
    | match_expr { $1 }
    | comparison_expr { $1 }

comparison_expr:
    | comparison_expr Equal additive_expr { Binary { binary_op = OpEqual; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr NotEqual additive_expr { Binary { binary_op = OpNotEqual; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr Less additive_expr { Binary { binary_op = OpLess; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | comparison_expr Greater additive_expr { Binary { binary_op = OpGreater; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | cons_expr { $1 }

cons_expr:
    | additive_expr Cons cons_expr { Binary { binary_op = OpCons; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | additive_expr { $1 }

additive_expr:
    | additive_expr Plus multiplicative_expr { Binary { binary_op = OpAdd; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | additive_expr Minus multiplicative_expr { Binary { binary_op = OpSub; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | multiplicative_expr { $1 }

multiplicative_expr:
    | multiplicative_expr Asterisk primary_expr { Binary { binary_op = OpMul; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | multiplicative_expr Slash primary_expr { Binary { binary_op = OpDiv; left = $1; right = $3; binary_span = merge (`Expr $1) (`Expr $3) } }
    | call_expr { $1 }  %prec BelowApp

call_expr:
    | call_expr LeftParen RightParen { Apply ($1, [], merge (`Expr $1) (`Span $3)) }
    | call_expr LeftParen arg_list RightParen { Apply ($1, $3, merge (`Expr $1) (`Span $4)) }
    | member_expr { $1 }

member_expr:
    | member_expr Dot Identifier { 
        let (name, span) = $3 in 
        MemberAccess ($1, name, merge (`Expr $1) (`Span span)) 
      }
    | primary_expr { $1 }

primary_expr:
    | Integer { let (i, span) = $1 in Literal (LInt i, span) }
    | FloatingPoint { let (f, span) = $1 in Literal (LFloat f, span) }
    | String { let (s, span) = $1 in Literal (LString s, span) }
    | Boolean { let (b, span) = $1 in Literal (LBool b, span) }
    | Unit { let ((), span) = $1 in Literal (LUnit, span) }
    | Identifier { let (name, span) = $1 in Variable (name, span) }
    | LeftParen RightParen { Literal (LUnit, merge (`Span $1) (`Span $2)) }
    | LeftParen expr RightParen { $2 }
    | LeftBracket expr DotDot expr RightBracket { Range ($2, $4, merge (`Span $1) (`Span $5)) }
    | LeftBracket RightBracket { Literal (LNil, merge (`Span $1) (`Span $2)) }
    | LeftBracket arg_list RightBracket { desugar_list $3 $2 }
    | block_expr { $1 }

if_expr:
    | KwIf expr KwThen expr KwElse expr { If { cond = $2; then_ = $4; else_ = Some $6; if_span = merge (`Span $1) (`Expr $6) } }
    | KwIf expr KwThen expr %prec THEN { If { cond = $2; then_ = $4; else_ = None; if_span = merge (`Span $1) (`Expr $4) } }

block_expr:
    | LeftBrace expr_list RightBrace { Block ($2, merge (`Span $1) (`Span $3)) }

match_expr:
    | KwMatch expr LeftBrace match_cases RightBrace { Match { scrutinee = $2; cases = $4; match_span = merge (`Span $1) (`Span $5) } }

match_cases:
    | Pipe match_case { [$2] }
    | Pipe match_case match_cases { $2 :: $3 }

match_case:
    | pattern Arrow expr { { pattern = $1; guard = None; case_body = $3; case_span = merge (`Pattern $1) (`Expr $3) } }
    | pattern KwIf expr Arrow expr { { pattern = $1; guard = Some $3; case_body = $5; case_span = merge (`Pattern $1) (`Expr $5) } }

pattern:
    | cons_pattern { $1 }

cons_pattern:
    | primary_pattern Cons cons_pattern { PCons ($1, $3, merge (`Pattern $1) (`Pattern $3)) }
    | primary_pattern { $1 }

pattern_list:
    | pattern { [$1] }
    | pattern Comma pattern_list { $1 :: $3 }

primary_pattern:
    | Underscore { PWildcard $1 }
    | Identifier { let (name, span) = $1 in PVariable (name, span) }
    | Integer { let (i, span) = $1 in PIntLiteral (i, span) }
    | FloatingPoint { let (f, span) = $1 in PFloatLiteral (f, span) }
    | String { let (s, span) = $1 in PStringLiteral (s, span) }
    | Boolean { let (b, span) = $1 in PBooleanLiteral (b, span) }
    | LeftBracket RightBracket { PNil (merge (`Span $1) (`Span $2)) }
    | LeftParen pattern RightParen { $2 }
    | Identifier LeftParen RightParen {
        let (name, span) = $1 in
        PConstructor (name, [], merge (`Span span) (`Span $2))
    }
    | Identifier LeftParen pattern_list RightParen {
        let (name, span) = $1 in
        PConstructor (name, $3, merge (`Span span) (`Span $4))
    }

let_expr:
    | KwLet Identifier Equal expr {
        let (name, _) = $2 in
        Let { name; ty = None; let_body = $4; let_span = merge (`Span $1) (`Expr $4) }
      }
    | KwLet Identifier Colon type_expr Equal expr {
        let (name, _) = $2 in
        Let { name; ty = Some $4; let_body = $6; let_span = merge (`Span $1) (`Expr $6) }
      }
    | KwLet Identifier block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $3 in
        let lambda = Lambda { params = []; ret_ty = None; lambda_body = $3; is_recursive;
                              lambda_span = merge (`Span $1) (`Expr $3) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $1) (`Expr $3) }
      }
    | KwLet Identifier Arrow ret_type block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $5 in
        let lambda = Lambda { params = []; ret_ty = Some $4; lambda_body = $5; is_recursive;
                              lambda_span = merge (`Span $3) (`Expr $5) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $1) (`Expr $5) }
      }
    | KwLet Identifier LeftParen RightParen block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $5 in
        let lambda = Lambda { params = []; ret_ty = None; lambda_body = $5; is_recursive;
                              lambda_span = merge (`Span $3) (`Expr $5) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $1) (`Expr $5) }
      }
    | KwLet Identifier LeftParen param_list RightParen block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $6 in
        let lambda = Lambda { params = $4; ret_ty = None; lambda_body = $6; is_recursive;
                              lambda_span = merge (`Span $3) (`Expr $6) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $3) (`Expr $6) }
      }
    | KwLet Identifier LeftParen RightParen Arrow ret_type block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $7 in
        let lambda = Lambda { params = []; ret_ty = Some $6; lambda_body = $7; is_recursive;
                              lambda_span = merge (`Span $3) (`Expr $7) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $1) (`Expr $7) }
      }
    | KwLet Identifier LeftParen param_list RightParen Arrow ret_type block_expr {
        let (name, _) = $2 in
        let is_recursive = contains_variable name $8 in
        let lambda = Lambda { params = $4; ret_ty = Some $7; lambda_body = $8; is_recursive;
                              lambda_span = merge (`Span $3) (`Expr $8) } in
        Let { name; ty = None; let_body = lambda; let_span = merge (`Span $1) (`Expr $8) }
      }

ret_type:
    | ret_type_primary { $1 }
    | ret_type_primary Arrow ret_type { TyFunction ([$1], $3) }
    | LeftParen ret_type_list RightParen Arrow ret_type { TyFunction ($2, $5) }

ret_type_primary:
    | Quote { let (c, _) = $1 in TyVar (String.make 1 c) }
    | IntegerType { TyFamilyMeta { fid = -1; family = FInt; fcontents = ref None } }
    | I8Type { TyI8  }
    | I16Type { TyI16 }
    | I32Type { TyI32 }
    | I64Type { TyI64 }
    | FloatingPointType { TyFamilyMeta { fid = -1; family = FFloat; fcontents = ref None } }
    | F32Type { TyF32 }
    | F64Type { TyF64 }
    | StringType { TyString }
    | BooleanType { TyBool }
    | UnitType { TyUnit }
    | Identifier { let (name, _) = $1 in
        if String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0]
        then TyAdt name
        else TyVar name }
    | ret_type_primary ListType { TyList $1 }
    | LeftParen ret_type RightParen { $2 }

ret_type_list:
    | ret_type Comma ret_type { [$1; $3] }
    | ret_type Comma ret_type_list { $1 :: $3 }

type_expr:
    | type_primary Arrow type_expr { TyFunction ([$1], $3) }
    | LeftParen type_expr_list RightParen Arrow type_expr { TyFunction ($2, $5) }
    | type_primary { $1 }

type_primary:
    | Quote { let (c, _) = $1 in TyVar (String.make 1 c) }
    | IntegerType { TyFamilyMeta { fid = -1; family = FInt; fcontents = ref None } }
    | I8Type { TyI8  }
    | I16Type { TyI16 }
    | I32Type { TyI32 }
    | I64Type { TyI64 }
    | FloatingPointType { TyFamilyMeta { fid = -1; family = FFloat; fcontents = ref None } }
    | F32Type { TyF32 }
    | F64Type { TyF64 }
    | StringType { TyString }
    | BooleanType { TyBool }
    | UnitType { TyUnit }
    | Identifier { let (name, _) = $1 in
        if String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0]
        then TyAdt name
        else TyVar name }
    | type_primary ListType { TyList $1 }
    | LeftParen type_expr RightParen { $2 }

type_expr_list:
    | type_expr Comma type_expr { [$1; $3] }
    | type_expr Comma type_expr_list { $1 :: $3 }

param_type:
    | param_type_primary { $1 }
    | param_type_primary Arrow param_type { TyFunction ([$1], $3) }
    | LeftParen param_type_list RightParen Arrow param_type { TyFunction ($2, $5) }

param_list:
    | param_or_group { $1 }
    | param_or_group Comma param_list { $1 @ $3 }

param_type_primary:
    | Quote { let (c, _) = $1 in TyVar (String.make 1 c) }
    | IntegerType { TyFamilyMeta { fid = -1; family = FInt; fcontents = ref None } }
    | I8Type { TyI8  }
    | I16Type { TyI16 }
    | I32Type { TyI32 }
    | I64Type { TyI64 }
    | FloatingPointType { TyFamilyMeta { fid = -1; family = FFloat; fcontents = ref None } }
    | F32Type { TyF32 }
    | F64Type { TyF64 }
    | StringType { TyString }
    | BooleanType { TyBool }
    | UnitType { TyUnit }
    | Identifier { let (name, _) = $1 in
        if String.length name > 0 && Char.uppercase_ascii name.[0] = name.[0]
        then TyAdt name
        else TyVar name }
    | param_type_primary ListType { TyList $1 }
    | LeftParen param_type RightParen { $2 }

param_type_list:
    | param_type Comma param_type { [$1; $3] }
    | param_type Comma param_type_list { $1 :: $3 }

param_or_group:
    | param { [$1] }
    | param_type LeftBracket identifier_list RightBracket { List.map (fun (id, sp) -> { param_name = id; param_ty = Some $1; param_span = sp }) $3 }

param:
    | Identifier { let (name, span) = $1 in { param_name = name; param_ty = None; param_span = span } }
    | Identifier Colon param_type { let (name, span) = $1 in { param_name = name; param_ty = Some $3; param_span = span } }

identifier_list:
    | Identifier { [$1] }
    | Identifier Comma identifier_list { $1 :: $3 }

arg_list:
    | expr { [$1] }
    | expr Comma arg_list { $1 :: $3 }