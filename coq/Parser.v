From Stdlib Require Import Strings.String.
From Stdlib Require Import ZArith.ZArith.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Bool.Bool.
Import ListNotations.

Require Import Token.
Require Import Ast.

Definition tokens := list token.

Inductive parse_result (A : Type) : Type :=
  | ParseOk (value : A) (remaining : tokens)
  | ParseErr (msg : string).

Arguments ParseOk {A}.
Arguments ParseErr {A}.

Definition peek (ts : tokens) : option token :=
  match ts with
  | [] => None
  | t :: _ => Some t
  end.

Fixpoint parse_ty (fuel : nat) (ts : tokens) : parse_result ty :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TInt :: ts1 => ParseOk TyInt ts1
  | TFloat :: ts1 => ParseOk TyFloat ts1
  | TString :: ts1 => ParseOk TyString ts1
  | TBool :: ts1 => ParseOk TyBool ts1
  | TUnit :: ts1 => ParseOk TyUnit ts1
  | TList :: ts1 =>
      match parse_ty f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk inner ts2 => ParseOk (TyList inner) ts2
      end
  | TIdent nm :: ts1 => ParseOk (TyVar nm) ts1
  | TLParen :: ts1 =>
      match parse_ty f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk t ts2 =>
          match ts2 with
          | TRParen :: ts3 => ParseOk t ts3
          | _              => ParseErr "expected ')' after type"
          end
      end
  | _ => ParseErr "expected type"
  end
  end.

Fixpoint parse_primary_pattern (fuel : nat) (ts : tokens) : parse_result pattern :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TWildcard :: ts1 => ParseOk PWildcard ts1
  | TTrue :: ts1 => ParseOk (PBoolLit true)  ts1
  | TFalse :: ts1 => ParseOk (PBoolLit false) ts1
  | TIntLit n :: ts1 => ParseOk (PIntLit n)  ts1
  | TFloatLit s :: ts1 => ParseOk (PFloatLit s) ts1
  | TStringLit s :: ts1 => ParseOk (PStringLit s) ts1
  | TLBracket :: TRBracket :: ts1 => ParseOk PNil ts1
  | TLParen :: TRParen :: ts1 => ParseOk PWildcard ts1
  | TLParen :: ts1 =>
      match parse_pattern f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk p ts2 =>
          match ts2 with
          | TRParen :: ts3 => ParseOk p ts3
          | _ => ParseErr "expected ')' in pattern"
          end
      end
  | TIdent nm :: TLParen :: TRParen :: ts1 =>
      ParseOk (PConstructor nm []) ts1
  | TIdent nm :: TLParen :: ts1 =>
      match parse_pattern_list f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk ps ts2 =>
          match ts2 with
          | TRParen :: ts3 => ParseOk (PConstructor nm ps) ts3
          | _ => ParseErr "expected ')' in constructor pattern"
          end
      end
  | TIdent nm :: ts1 => ParseOk (PVariable nm) ts1
  | _ => ParseErr "expected pattern"
  end
  end

with parse_pattern (fuel : nat) (ts : tokens) : parse_result pattern :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_primary_pattern f ts with
  | ParseErr e => ParseErr e
  | ParseOk p ts1 =>
      match ts1 with
      | TCons :: ts2 =>
          match parse_pattern f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk p2 ts3 => ParseOk (PCons p p2) ts3
          end
      | _ => ParseOk p ts1
      end
  end
  end

with parse_pattern_list (fuel : nat) (ts : tokens) : parse_result (list pattern) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_pattern f ts with
  | ParseErr e => ParseErr e
  | ParseOk p ts1 =>
      match ts1 with
      | TComma :: ts2 =>
          match parse_pattern_list f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk ps ts3 => ParseOk (p :: ps) ts3
          end
      | _ => ParseOk [p] ts1
      end
  end
  end.

Definition parse_param (fuel : nat) (ts : tokens) : parse_result param :=
  match ts with
  | TIdent nm :: TColon :: ts1 =>
      match parse_ty fuel ts1 with
      | ParseErr e => ParseErr e
      | ParseOk t ts2 => ParseOk (mkParam nm (Some t)) ts2
      end
  | TIdent nm :: ts1 => ParseOk (mkParam nm None) ts1
  | _ => ParseErr "expected parameter"
  end.

Fixpoint parse_param_list (fuel : nat) (ts : tokens) : parse_result (list param) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_param f ts with
  | ParseErr e => ParseErr e
  | ParseOk p ts1 =>
      match ts1 with
      | TComma :: ts2 =>
          match parse_param_list f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk ps ts3 => ParseOk (p :: ps) ts3
          end
      | _ => ParseOk [p] ts1
      end
  end
  end.

Fixpoint parse_expr (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TLet :: TIdent nm :: TEquals :: ts1 =>
      match parse_expr f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk body ts2 => ParseOk (LetE nm None body) ts2
      end
  | TLet :: TIdent nm :: TColon :: ts1 =>
      match parse_ty f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk t ts2 =>
          match ts2 with
          | TEquals :: ts3 =>
              match parse_expr f ts3 with
              | ParseErr e       => ParseErr e
              | ParseOk body ts4 => ParseOk (LetE nm (Some t) body) ts4
              end
          | _ => ParseErr "expected '=' after type annotation"
          end
      end
  | TLet :: TIdent nm :: TLBrace :: ts1 =>
      match parse_block_body f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk body ts2 => ParseOk (LetE nm None (Lam [] None body (contains_variable nm body))) ts2
      end
  | TLet :: TIdent nm :: TLParen :: TRParen :: ts1 =>
      let (ret_ty, ts2) :=
        match ts1 with
        | TArrow :: ts1a =>
            match parse_ty f ts1a with
            | ParseOk t ts1b => (Some t, ts1b)
            | ParseErr _ => (None,   ts1)
            end
        | _ => (None, ts1)
        end
      in
      match parse_block f ts2 with
      | ParseErr e => ParseErr e
      | ParseOk body ts3 => ParseOk (LetE nm None (Lam [] ret_ty body (contains_variable nm body))) ts3
      end
  | TLet :: TIdent nm :: TLParen :: ts1 =>
      match parse_param_list f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk ps ts2 =>
          match ts2 with
          | TRParen :: ts3 =>
              let (ret_ty, ts4) :=
                match ts3 with
                | TArrow :: ts3a =>
                    match parse_ty f ts3a with
                    | ParseOk t ts3b => (Some t, ts3b)
                    | ParseErr _ => (None,   ts3)
                    end
                | _ => (None, ts3)
                end
              in
              match parse_block f ts4 with
              | ParseErr e => ParseErr e
              | ParseOk body ts5 => ParseOk (LetE nm None (Lam ps ret_ty body (contains_variable nm body))) ts5
              end
          | _ => ParseErr "expected ')' after params"
          end
      end
  | TIf :: ts1 =>
      match parse_expr f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk cond ts2 =>
          match ts2 with
          | TThen :: ts3 =>
              match parse_expr f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk then_ ts4 =>
                  match ts4 with
                  | TElse :: ts5 =>
                      match parse_expr f ts5 with
                      | ParseErr e        => ParseErr e
                      | ParseOk else_ ts6 => ParseOk (IfE cond then_ (Some else_)) ts6
                      end
                  | _ => ParseOk (IfE cond then_ None) ts4
                  end
              end
          | _ => ParseErr "expected 'then'"
          end
      end
  | TMatch :: ts1 =>
      match parse_expr f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk scrut ts2 =>
          match ts2 with
          | TLBrace :: ts3 =>
              match parse_match_cases f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk cases ts4 =>
                  match ts4 with
                  | TRBrace :: ts5 => ParseOk (MatchE scrut cases) ts5
                  | _ => ParseErr "expected '}' after match cases"
                  end
              end
          | _ => ParseErr "expected '{' after match scrutinee"
          end
      end

  | _ => parse_comparison f ts
  end
  end

with parse_comparison (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_cons f ts with
  | ParseErr e => ParseErr e
  | ParseOk lhs ts1 =>
      match ts1 with
      | TEquals :: ts2 =>
          match parse_cons f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk rhs ts3 => ParseOk (BinOp OpEqual lhs rhs) ts3
          end
      | TNotEq :: ts2 =>
          match parse_cons f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk rhs ts3 => ParseOk (BinOp OpNotEqual lhs rhs) ts3
          end
      | TLt :: ts2 =>
          match parse_cons f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk rhs ts3 => ParseOk (BinOp OpLess lhs rhs) ts3
          end
      | TGt :: ts2 =>
          match parse_cons f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk rhs ts3 => ParseOk (BinOp OpGreater lhs rhs) ts3
          end
      | _ => ParseOk lhs ts1
      end
  end
  end

with parse_cons (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_additive f ts with
  | ParseErr e => ParseErr e
  | ParseOk lhs ts1 =>
      match ts1 with
      | TCons :: ts2 =>
          match parse_cons f ts2 with
          | ParseErr e => ParseErr e
          | ParseOk rhs ts3 => ParseOk (BinOp OpCons lhs rhs) ts3
          end
      | _ => ParseOk lhs ts1
      end
  end
  end

with parse_additive (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_multiplicative f ts with
  | ParseErr e => ParseErr e
  | ParseOk lhs ts1 => parse_additive_rest f lhs ts1
  end
  end

with parse_additive_rest (fuel : nat) (lhs : expr) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TPlus  :: ts1 =>
      match parse_multiplicative f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk rhs ts2 => parse_additive_rest f (BinOp OpAdd lhs rhs) ts2
      end
  | TMinus :: ts1 =>
      match parse_multiplicative f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk rhs ts2 => parse_additive_rest f (BinOp OpSub lhs rhs) ts2
      end
  | _ => ParseOk lhs ts
  end
  end

with parse_multiplicative (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_call f ts with
  | ParseErr e => ParseErr e
  | ParseOk lhs ts1 => parse_multiplicative_rest f lhs ts1
  end
  end

with parse_multiplicative_rest (fuel : nat) (lhs : expr) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TStar  :: ts1 =>
      match parse_call f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk rhs ts2 => parse_multiplicative_rest f (BinOp OpMul lhs rhs) ts2
      end
  | TSlash :: ts1 =>
      match parse_call f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk rhs ts2 => parse_multiplicative_rest f (BinOp OpDiv lhs rhs) ts2
      end
  | _ => ParseOk lhs ts
  end
  end

with parse_call (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_member f ts with
  | ParseErr e => ParseErr e
  | ParseOk callee ts1 => parse_call_rest f callee ts1
  end
  end

with parse_call_rest (fuel : nat) (callee : expr) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TLParen :: TRParen :: ts1 =>
      parse_call_rest f (App callee []) ts1
  | TLParen :: ts1 =>
      match parse_arg_list f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk args ts2 =>
          match ts2 with
          | TRParen :: ts3 => parse_call_rest f (App callee args) ts3
          | _ => ParseErr "expected ')' after arguments"
          end
      end
  | _ => ParseOk callee ts
  end
  end

with parse_member (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_primary f ts with
  | ParseErr e => ParseErr e
  | ParseOk obj ts1 =>
      let fix member_rest (g : nat) (o : expr) (s : tokens) : parse_result expr :=
        match g with O => ParseOk o s | S g' =>
        match s with
        | TDot :: TIdent field :: s1 =>
            member_rest g' (MemberAccess o field) s1
        | _ => ParseOk o s
        end
        end
      in
      member_rest f obj ts1
  end
  end

with parse_primary (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TIntLit n :: ts1 => ParseOk (Lit (LInt n))     ts1
  | TFloatLit s :: ts1 => ParseOk (Lit (LFloat s))   ts1
  | TStringLit s :: ts1 => ParseOk (Lit (LString s))  ts1
  | TTrue :: ts1 => ParseOk (Lit (LBool true))  ts1
  | TFalse :: ts1 => ParseOk (Lit (LBool false)) ts1
  | TLParen :: TRParen :: ts1 => ParseOk (Lit LUnit) ts1
  | TLBracket :: TRBracket :: ts1 => ParseOk (Lit LNil) ts1
  | TLBracket :: ts1 =>
      match parse_expr f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk e1 ts2 =>
          match ts2 with
          | TDotDot :: ts3 =>
              match parse_expr f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk e2 ts4 =>
                  match ts4 with
                  | TRBracket :: ts5 => ParseOk (Range e1 e2) ts5
                  | _ => ParseErr "expected ']' after range"
                  end
              end
          | TComma :: ts3 =>
              match parse_arg_list f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk rest ts4 =>
                  match ts4 with
                  | TRBracket :: ts5 => ParseOk (desugar_list (e1 :: rest)) ts5
                  | _  => ParseErr "expected ']' after list"
                  end
              end
          | TRBracket :: ts3 => ParseOk (desugar_list [e1]) ts3
          | _ => ParseErr "expected ',', '..' or ']' in list/range"
          end
      end
  | TLParen :: ts1 =>
      match parse_expr f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk e ts2 =>
          match ts2 with
          | TRParen :: ts3 => ParseOk e ts3
          | _ => ParseErr "expected ')'"
          end
      end
  | TLBrace :: ts1 =>
      parse_block_body f ts1
  | TIdent nm :: ts1 =>
      ParseOk (Var nm) ts1
  | _ => ParseErr "expected expression"
  end
  end

with parse_block (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TLBrace :: ts1 => parse_block_body f ts1
  | _ => ParseErr "expected '{'"
  end
  end

with parse_block_body (fuel : nat) (ts : tokens) : parse_result expr :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_expr_list f ts with
  | ParseErr e => ParseErr e
  | ParseOk exprs ts1 =>
      match ts1 with
      | TRBrace :: ts2 => ParseOk (Block exprs) ts2
      | _ => ParseErr "expected '}'"
      end
  end
  end

with parse_expr_list (fuel : nat) (ts : tokens) : parse_result (list expr) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TRBrace :: _ => ParseOk [] ts
  | TEOF :: _ => ParseOk [] ts
  | _ =>
      match parse_expr f ts with
      | ParseErr e => ParseErr e
      | ParseOk e ts1 =>
          match parse_expr_list f ts1 with
          | ParseErr e2 => ParseErr e2
          | ParseOk es ts2 => ParseOk (e :: es) ts2
          end
      end
  end
  end

with parse_arg_list (fuel : nat) (ts : tokens) : parse_result (list expr) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match parse_expr f ts with
  | ParseErr e => ParseErr e
  | ParseOk e ts1 =>
      match ts1 with
      | TComma :: ts2 =>
          match parse_arg_list f ts2 with
          | ParseErr e2 => ParseErr e2
          | ParseOk es ts3 => ParseOk (e :: es) ts3
          end
      | _ => ParseOk [e] ts1
      end
  end
  end

with parse_match_cases (fuel : nat) (ts : tokens) : parse_result (list match_case) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TRBrace :: _ => ParseOk [] ts
  | TPipe :: ts1 =>
      match parse_pattern f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk pat ts2 =>
          match ts2 with
          | TIf :: ts3 =>
              match parse_expr f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk guard ts4 =>
                  match ts4 with
                  | TArrow :: ts5 =>
                      match parse_expr f ts5 with
                      | ParseErr e => ParseErr e
                      | ParseOk body ts6 =>
                          match parse_match_cases f ts6 with
                          | ParseErr e => ParseErr e
                          | ParseOk cases ts7 =>
                              ParseOk (MkCase pat (Some guard) body :: cases) ts7
                          end
                      end
                  | _ => ParseErr "expected '->' in match case"
                  end
              end
          | TArrow :: ts3 =>
              match parse_expr f ts3 with
              | ParseErr e => ParseErr e
              | ParseOk body ts4 =>
                  match parse_match_cases f ts4 with
                  | ParseErr e => ParseErr e
                  | ParseOk cases ts5 => ParseOk (MkCase pat None body :: cases) ts5
                  end
              end
          | _ => ParseErr "expected '->' in match case"
          end
      end
  | _ => ParseOk [] ts
  end
  end.

Fixpoint parse_imports (fuel : nat) (ts : tokens) : parse_result (list import) :=
  match fuel with O => ParseErr "out of fuel" | S f =>
  match ts with
  | TImport :: TStringLit path :: ts1 =>
      match parse_imports f ts1 with
      | ParseErr e => ParseErr e
      | ParseOk imps ts2  =>
          ParseOk (mkImport path "" :: imps) ts2
      end
  | _ => ParseOk [] ts
  end
  end.

Definition parse (ts : tokens) : parse_result program :=
  let fuel := (List.length ts + 1) * 16 in
  match parse_imports fuel ts with
  | ParseErr e => ParseErr e
  | ParseOk imps ts1 =>
      match parse_expr_list fuel ts1 with
      | ParseErr e => ParseErr e
      | ParseOk body ts2 => ParseOk (mkProgram imps [] body) ts2
      end
  end.
