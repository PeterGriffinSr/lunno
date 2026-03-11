From Stdlib Require Import Strings.String.
From Stdlib Require Import ZArith.ZArith.
From Stdlib Require Import Lists.List.
Import ListNotations.

Inductive ty : Type :=
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyVar (name : string)
  | TyList (inner : ty)
  | TyFun (params : list ty) (ret : ty)
  | TyAdt (name : string).

Inductive literal : Type :=
  | LInt (n : Z)
  | LFloat (s : string)
  | LString (s : string)
  | LBool (b : bool)
  | LUnit
  | LNil.

Inductive binary_op : Type :=
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEqual
  | OpNotEqual
  | OpLess
  | OpGreater
  | OpCons.

Inductive unary_op : Type :=
  | OpNegate.

Inductive pattern : Type :=
  | PWildcard
  | PVariable (name : string)
  | PIntLit (n : Z)
  | PFloatLit (s : string)
  | PStringLit (s : string)
  | PBoolLit (b : bool)
  | PNil
  | PCons (head : pattern) (tail : pattern)
  | PConstructor (name : string) (args : list pattern).

Record param : Type := mkParam {
  param_name : string;
  param_ty   : option ty;
}.

Inductive expr : Type :=
  | Lit (l : literal)
  | Var (name : string)
  | Lam (params : list param) (ret_ty : option ty) (body : expr) (is_recursive : bool)
  | App (f : expr) (args : list expr)
  | LetE (name : string) (ty : option ty) (body : expr)
  | IfE (cond : expr) (then_ : expr) (else_ : option expr)
  | MatchE (scrutinee : expr) (cases : list match_case)
  | Block (exprs : list expr)
  | BinOp (op : binary_op) (left : expr) (right : expr)
  | UnOp (op : unary_op)  (e : expr)
  | MemberAccess (obj : expr) (field : string)
  | Range (lo : expr) (hi : expr)
  | Constructor (name : string) (args : list expr)

with match_case : Type :=
  | MkCase (pat : pattern) (guard : option expr) (body : expr).

Record import : Type := mkImport {
  import_module : string;
  import_item   : string;
}.

Record variant : Type := mkVariant {
  variant_name   : string;
  variant_fields : list ty;
}.

Record type_decl : Type := mkTypeDecl {
  type_name : string;
  variants  : list variant;
}.

Record program : Type := mkProgram {
  imports    : list import;
  type_decls : list type_decl;
  body       : list expr;
}.

Definition empty_program : program :=
  mkProgram [] [] [].

Definition is_empty_program (p : program) : bool :=
  match p.(body) with
  | [] => true
  | _  => false
  end.

Fixpoint desugar_list (elems : list expr) : expr :=
  match elems with
  | [] => Lit LNil
  | e :: rest => BinOp OpCons e (desugar_list rest)
  end.

Fixpoint contains_variable (name : string) (e : expr) : bool :=
  match e with
  | Lit _ => false
  | Var nm => String.eqb nm name
  | Lam _ _ body _ => contains_variable name body
  | App f args =>
      orb (contains_variable name f)
          (existsb (contains_variable name) args)
  | LetE _ _ body => contains_variable name body
  | IfE cond then_ else_ =>
      orb (contains_variable name cond)
      (orb (contains_variable name then_)
           (match else_ with
            | Some e => contains_variable name e
            | None => false
            end))
  | MatchE scrut cases =>
      orb (contains_variable name scrut)
          (existsb (fun c =>
            match c with
            | MkCase _ guard body =>
                orb (contains_variable name body)
                    (match guard with
                     | Some g => contains_variable name g
                     | None   => false
                     end)
            end) cases)
  | Block exprs => existsb (contains_variable name) exprs
  | BinOp _ l r => orb (contains_variable name l) (contains_variable name r)
  | UnOp _ e => contains_variable name e
  | MemberAccess obj _ => contains_variable name obj
  | Range lo hi => orb (contains_variable name lo) (contains_variable name hi)
  | Constructor _ args => existsb (contains_variable name) args
  end.

Lemma contains_variable_lit : forall nm l,
  contains_variable nm (Lit l) = false.
Proof. intros nm l. reflexivity. Qed.

Lemma contains_variable_var_same : forall nm,
  contains_variable nm (Var nm) = true.
Proof. intros nm. simpl. rewrite String.eqb_refl. reflexivity. Qed.

Lemma contains_variable_var_diff : forall nm1 nm2,
  nm1 <> nm2 -> contains_variable nm1 (Var nm2) = false.
Proof.
  intros nm1 nm2 H. simpl.
  rewrite <- String.eqb_neq in H. rewrite String.eqb_sym. rewrite H. reflexivity.
Qed.

Lemma desugar_nil : desugar_list [] = Lit LNil.
Proof. reflexivity. Qed.

Lemma desugar_singleton : forall e,
  desugar_list [e] = BinOp OpCons e (Lit LNil).
Proof. intros e. reflexivity. Qed.

Lemma desugar_cons : forall e es,
  desugar_list (e :: es) = BinOp OpCons e (desugar_list es).
Proof. intros e es. reflexivity. Qed.

Lemma empty_program_body : (empty_program).(body) = [].
Proof. reflexivity. Qed.

Lemma is_empty_empty : is_empty_program empty_program = true.
Proof. reflexivity. Qed.