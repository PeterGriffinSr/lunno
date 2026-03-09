open OUnit2
open Lunno_common
open Test_helpers

let tc_test_int_literal ctxt = assert_ok ~ctxt "42"
let tc_test_float_literal ctxt = assert_ok ~ctxt "3.14"
let tc_test_string_literal ctxt = assert_ok ~ctxt "\"hello\""
let tc_test_bool_literal ctxt = assert_ok ~ctxt "true"
let tc_test_unit_literal ctxt = assert_ok ~ctxt "let f -> unit { f() }"
let tc_test_let_int ctxt = assert_ok ~ctxt "let x: int = 42"
let tc_test_let_float ctxt = assert_ok ~ctxt "let x: float = 3.14"
let tc_test_let_string ctxt = assert_ok ~ctxt "let x: string = \"hello\""
let tc_test_let_bool ctxt = assert_ok ~ctxt "let x: bool = true"

let tc_test_let_annotation_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "let x: int = 3.14"

let tc_test_let_annotation_mismatch_string ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "let x: string = 42"

let tc_test_let_no_annotation ctxt = assert_ok ~ctxt "let x = 42"

let tc_test_let_duplicate_top_level ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined) "let x = 1\nlet x = 2"

let tc_test_let_duplicate_in_block ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined)
    "let f() { let x = 1\n let x = 2\n x }"

let tc_test_let_param_duplicate ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined)
    "let f(x: int) -> int { let x = 2\n x }"

let tc_test_undefined_variable ctxt =
  assert_error ~ctxt ~code:(Some E_Type_UndefinedVariable) "x"

let tc_test_variable_in_scope ctxt = assert_ok ~ctxt "let x = 42\nx"

let tc_test_variable_used_before_definition ctxt =
  assert_error ~ctxt ~code:(Some E_Type_UndefinedVariable) "let f() { x }"

let tc_test_simple_function ctxt = assert_ok ~ctxt "let f(x: int) -> int { x }"

let tc_test_function_return_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let f(x: int) -> int { \"hello\" }"

let tc_test_function_missing_annotation ctxt =
  assert_ok ~ctxt "let f(x) -> int { 42 }"

let tc_test_function_no_return_annotation ctxt =
  assert_ok ~ctxt "let f(x: int) { x }"

let tc_test_multi_param_function ctxt =
  assert_ok ~ctxt "let f(x: int, y: int) -> int { x + y }"

let tc_test_function_returning_bool ctxt =
  assert_ok ~ctxt "let is_pos(x: int) -> bool { x > 0 }"

let tc_test_function_returning_float ctxt =
  assert_ok ~ctxt "let double(x: float) -> float { x + x }"

let tc_test_function_returning_string ctxt =
  assert_ok ~ctxt "let greet(s: string) -> string { s }"

let tc_test_simple_application ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { x }\nf(42)"

let tc_test_arity_mismatch_too_few ctxt =
  assert_error ~ctxt ~code:(Some E_Type_ArityMismatch)
    "let f(x: int, y: int) -> int { x + y }\nf(1)"

let tc_test_arity_mismatch_too_many ctxt =
  assert_error ~ctxt ~code:(Some E_Type_ArityMismatch)
    "let f(x: int) -> int { x }\nf(1, 2)"

let tc_test_apply_wrong_arg_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let f(x: int) -> int { x }\nf(3.14)"

let tc_test_apply_not_a_function ctxt =
  assert_error ~ctxt ~code:(Some E_Type_NotAFunction) "let x = 42\nx(1)"

let tc_test_apply_zero_args_ok ctxt = assert_ok ~ctxt "let f -> int { 42 }\nf()"
let tc_test_int_add ctxt = assert_ok ~ctxt "1 + 2"
let tc_test_float_add ctxt = assert_ok ~ctxt "1.0 + 2.0"

let tc_test_int_float_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "1 + 2.0"

let tc_test_float_int_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "1.0 + 2"

let tc_test_int_sub ctxt = assert_ok ~ctxt "5 - 3"
let tc_test_int_mul ctxt = assert_ok ~ctxt "2 * 3"
let tc_test_int_div ctxt = assert_ok ~ctxt "10 / 2"
let tc_test_float_mul ctxt = assert_ok ~ctxt "2.0 * 3.0"

let tc_test_string_add_invalid ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "\"a\" + \"b\""

let tc_test_negate_int ctxt = assert_ok ~ctxt "let f(x: int) -> int { 0 - x }"

let tc_test_negate_float ctxt =
  assert_ok ~ctxt "let f(x: float) -> float { 0.0 - x }"

let tc_test_negate_bool_invalid ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let f(x: bool) -> int { x + 1 }"

let tc_test_int_less ctxt = assert_ok ~ctxt "1 < 2"
let tc_test_int_greater ctxt = assert_ok ~ctxt "2 > 1"
let tc_test_float_less ctxt = assert_ok ~ctxt "1.0 < 2.0"
let tc_test_int_equal ctxt = assert_ok ~ctxt "1 = 1"
let tc_test_int_not_equal ctxt = assert_ok ~ctxt "1 <> 2"
let tc_test_bool_equal ctxt = assert_ok ~ctxt "true = false"
let tc_test_string_equal ctxt = assert_ok ~ctxt "\"a\" = \"b\""

let tc_test_compare_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "1 < 2.0"

let tc_test_equal_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "1 = true"

let tc_test_simple_if ctxt = assert_ok ~ctxt "if true then 1 else 0"

let tc_test_if_with_condition ctxt =
  assert_ok ~ctxt "let x = 5\nif x > 0 then x else 0"

let tc_test_if_branch_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_IfBranchMismatch)
    "if true then 1 else \"hello\""

let tc_test_if_condition_not_bool ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch) "if 1 then 1 else 0"

let tc_test_if_missing_else ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MissingElseBranch)
    "let f(x: int) -> int { if x > 0 then x }"

let tc_test_if_float_branches ctxt = assert_ok ~ctxt "if true then 1.0 else 2.0"

let tc_test_if_string_branches ctxt =
  assert_ok ~ctxt "if true then \"a\" else \"b\""

let tc_test_nested_if ctxt =
  assert_ok ~ctxt "if true then (if false then 1 else 2) else 3"

let tc_test_if_in_function ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { if x > 0 then x else 0 }"

let tc_test_match_int ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { match x { | 0 -> 0 | _ -> 1 } }"

let tc_test_match_bool ctxt =
  assert_ok ~ctxt
    "let f(b: bool) -> int { match b { | true -> 1 | false -> 0 } }"

let tc_test_match_branch_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MatchBranchMismatch)
    "let f(x: int) -> int { match x { | 0 -> 1 | _ -> \"hello\" } }"

let tc_test_match_pattern_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_PatternTypeMismatch)
    "let f(x: int) -> int { match x { | \"hello\" -> 1 } }"

let tc_test_match_wildcard ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { match x { | _ -> 42 } }"

let tc_test_match_variable_pattern ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { match x { | n -> n } }"

let tc_test_match_variable_used_in_body ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { match x { | n -> n + 1 } }"

let tc_test_match_with_guard ctxt =
  assert_ok ~ctxt
    "let f(x: int) -> int { match x { | n if n > 0 -> n | _ -> 0 } }"

let tc_test_match_guard_not_bool ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let f(x: int) -> int { match x { | n if n + 1 -> n | _ -> 0 } }"

let tc_test_match_nil_pattern ctxt =
  assert_ok ~ctxt
    "let f(xs: int list) -> int { match xs { | [] -> 0 | _ -> 1 } }"

let tc_test_match_cons_pattern ctxt =
  assert_ok ~ctxt
    "let f(xs: int list) -> int { match xs { | x :: _ -> x | [] -> 0 } }"

let tc_test_match_string ctxt =
  assert_ok ~ctxt
    "let f(s: string) -> int { match s { | \"hello\" -> 1 | _ -> 0 } }"

let tc_test_recursive_function ctxt =
  assert_ok ~ctxt
    "let fib(n: int) -> int { match n { | 0 -> 0 | 1 -> 1 | m -> fib(m - 1) + \
     fib(m - 2) } }"

let tc_test_recursive_missing_return_annotation ctxt =
  assert_ok ~ctxt "let f(n) { match n { | 0 -> 0 | m -> f(m - 1) } }"

let tc_test_nested_recursive_function ctxt =
  assert_ok ~ctxt
    {|let fib(n: int) -> int {
        let fib_aux(int[m, a, b]) -> int {
          match m {
            | 0 -> a
            | _ -> fib_aux(m - 1, b, a + b)
          }
        }
        fib_aux(n, 0, 1)
      }|}

let tc_test_block_returns_last ctxt =
  assert_ok ~ctxt "let f -> int { let x = 1\n let y = 2\n x + y }"

let tc_test_block_let_scoping ctxt =
  assert_ok ~ctxt "let f -> int { let x = 10\n x }"

let tc_test_block_sequential_lets ctxt =
  assert_ok ~ctxt "let f -> int { let x = 1\n let y = x + 1\n y }"

let tc_test_block_let_uses_previous ctxt =
  assert_ok ~ctxt "let f(x: int) -> int { let y = x + 1\n let z = y + 1\n z }"

let tc_test_match_nil ctxt =
  assert_ok ~ctxt
    "let f(xs: int list) -> int { match xs { | [] -> 0 | _ -> 1 } }"

let tc_test_match_cons_head ctxt =
  assert_ok ~ctxt
    "let f(xs: int list) -> int { match xs { | x :: _ -> x | [] -> 0 } }"

let tc_test_match_cons_wrong_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_PatternTypeMismatch)
    "let f(xs: int list) -> int { match xs { | \"a\" :: _ -> 1 | _ -> 0 } }"

let tc_test_function_as_arg ctxt =
  assert_ok ~ctxt
    "let apply(f: (int) -> int, x: int) -> int { f(x) }\n\
     let double(x: int) -> int { x * 2 }\n\
     apply(double, 5)"

let tc_test_function_return_type ctxt =
  assert_ok ~ctxt
    "let add(x: int) -> (int) -> int { let inner(y: int) -> int { x + y }\n\
    \ inner }"

let tc_test_primed_name ctxt = assert_ok ~ctxt "let x = 1\nlet x' = 2\nx + x'"

let tc_test_double_primed_name ctxt =
  assert_ok ~ctxt "let x = 1\nlet x' = 2\nlet x'' = 3\nx + x' + x''"

let tc_test_adt_simple ctxt =
  assert_ok ~ctxt
    {|data Color = { | Red | Green | Blue }
      let c: Color = Red|}

let tc_test_adt_constructor_with_field ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let x: Option = Some(42)|}

let tc_test_adt_constructor_wrong_arity ctxt =
  assert_error ~ctxt ~code:(Some E_Type_ArityMismatch)
    {|data Option = { | Some(int) | None }
      let x: Option = Some(1, 2)|}

let tc_test_adt_constructor_wrong_field_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|data Option = { | Some(int) | None }
      let x: Option = Some("oops")|}

let tc_test_adt_match_constructor ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let unwrap(o: Option) -> int {
        match o {
          | Some(n) -> n
          | None    -> 0
        }
      }|}

let tc_test_adt_match_branch_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MatchBranchMismatch)
    {|data Option = { | Some(int) | None }
      let f(o: Option) -> int {
        match o {
          | Some(n) -> n
          | None    -> "missing"
        }
      }|}

let tc_test_adt_match_pattern_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_PatternTypeMismatch)
    {|data Option = { | Some(int) | None }
      let f(x: int) -> int {
        match x { | Some(n) -> n }
      }|}

let tc_test_adt_nested_constructor ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let double_wrap(n: int) -> Option { Some(n) }
      let x: Option = double_wrap(42)|}

let tc_test_adt_result_type ctxt =
  assert_ok ~ctxt
    {|data Result = { | Ok(int) | Err(string) }
      let safe_div(a: int, b: int) -> Result {
        if b = 0 then Err("division by zero") else Ok(a / b)
      }|}

let tc_test_adt_recursive_match ctxt =
  assert_ok ~ctxt
    {|data Tree = { | Leaf | Node(int, Tree, Tree) }
      let depth(t: Tree) -> int {
        match t {
          | Leaf           -> 0
          | Node(_, l, r)  -> {
              let ld = depth(l)
              let rd = depth(r)
              if ld > rd then ld + 1 else rd + 1
            }
        }
      }|}

let tc_test_adt_multi_field_pattern ctxt =
  assert_ok ~ctxt
    {|data Pair = { | Pair(int, int) }
      let fst(p: Pair) -> int {
        match p { | Pair(a, _) -> a }
      }|}

let tc_test_adt_undefined_constructor ctxt =
  assert_error ~ctxt ~code:(Some E_Type_UndefinedVariable)
    {|data Option = { | Some(int) | None }
      let x: Option = Missing(42)|}

let tc_cons_int_list ctxt =
  assert_ok ~ctxt "let xs: int list = 1 :: 2 :: 3 :: []"

let tc_cons_onto_empty ctxt = assert_ok ~ctxt "let xs: int list = 42 :: []"

let tc_cons_variable_onto_list ctxt =
  assert_ok ~ctxt {|let f(x: int, xs: int list) -> int list { x :: xs }|}

let tc_cons_wrong_element_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(xs: int list) -> int list { "hello" :: xs }|}

let tc_cons_wrong_tail_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(xs: string list) -> int list { 1 :: xs }|}

let tc_cons_nested_list ctxt =
  assert_ok ~ctxt {|let f(xs: int list list) -> int list list { [] :: xs }|}

let tc_cons_in_match_body ctxt =
  assert_ok ~ctxt
    {|let f(xs: int list) -> int list {
        match xs {
          | [] -> []
          | x :: rest -> x :: rest
        }
      }|}

let tc_list_type_annotation_accepted ctxt =
  assert_ok ~ctxt "let xs: int list = []"

let tc_list_type_float_accepted ctxt =
  assert_ok ~ctxt "let xs: float list = 1.0 :: 2.0 :: []"

let tc_list_type_mismatch_cons_head ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let xs: int list = 1.0 :: []"

let tc_list_passed_to_function ctxt =
  assert_ok ~ctxt
    {|let sum(xs: int list) -> int {
        match xs { | [] -> 0 | x :: rest -> x + sum(rest) }
      }
      sum(1 :: 2 :: 3 :: [])|}

let tc_adt_three_variants ctxt =
  assert_ok ~ctxt
    {|data Shape = { | Circle(float) | Rect(float, float) | Point }
      let area(s: Shape) -> float {
        match s {
          | Circle(r)     -> r
          | Rect(w, h)    -> w + h
          | Point         -> 0.0
        }
      }|}

let tc_adt_three_field_constructor ctxt =
  assert_ok ~ctxt
    {|data Triple = { | Triple(int, int, int) }
      let sum3(t: Triple) -> int {
        match t { | Triple(a, b, c) -> a + b + c }
      }|}

let tc_adt_three_field_wrong_arity ctxt =
  assert_error ~ctxt ~code:(Some E_Type_ArityMismatch)
    {|data Triple = { | Triple(int, int, int) }
      let x: Triple = Triple(1, 2)|}

let tc_adt_nested_match ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let f(o: Option) -> int {
        match o {
          | Some(n) ->
              match n {
                | 0 -> 0
                | _ -> n
              }
          | None -> 0
        }
      }|}

let tc_adt_constructor_in_branch ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let default(o: Option, d: int) -> Option {
        match o {
          | None   -> Some(d)
          | Some(n) -> Some(n)
        }
      }|}

let tc_adt_field_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|data Wrapper = { | Wrap(int) }
      let x: Wrapper = Wrap(3.14)|}

let tc_adt_match_binds_fields ctxt =
  assert_ok ~ctxt
    {|data Pair = { | Pair(int, int) }
      let swap(p: Pair) -> Pair {
        match p { | Pair(a, b) -> Pair(b, a) }
      }|}

let tc_adt_match_wrong_field_count ctxt =
  assert_error ~ctxt ~code:(Some E_Type_ArityMismatch)
    {|data Pair = { | Pair(int, int) }
      let f(p: Pair) -> int {
        match p { | Pair(a) -> a }
      }|}

let tc_adt_recursive_type ctxt =
  assert_ok ~ctxt
    {|data List = { | Cons(int, List) | Nil }
      let length(xs: List) -> int {
        match xs {
          | Nil        -> 0
          | Cons(_, t) -> 1 + length(t)
        }
      }|}

let tc_adt_mutually_used_in_function ctxt =
  assert_ok ~ctxt
    {|data Color = { | Red | Green | Blue }
      let to_int(c: Color) -> int {
        match c { | Red -> 0 | Green -> 1 | Blue -> 2 }
      }
      let from_int(n: int) -> Color {
        match n { | 0 -> Red | 1 -> Green | _ -> Blue }
      }|}

let tc_if_missing_else_in_block ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MissingElseBranch)
    {|let f(x: int) -> int {
        let y = if x > 0 then x
        y
      }|}

let tc_if_missing_else_nested ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MissingElseBranch)
    {|let f(x: int) -> int {
        if x > 0 then
          if x > 10 then x
        else 0
      }|}

let tc_if_condition_is_function ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: int) -> int { x }
      if f then 1 else 0|}

let tc_if_condition_is_list ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    "let xs: int list = []\nif xs then 1 else 0"

let tc_if_both_branches_unit ctxt =
  assert_ok ~ctxt
    {|let f -> unit { f() }
      let g -> unit { g() }
      if true then f() else g()|}

let tc_match_all_branches_same_type ctxt =
  assert_ok ~ctxt
    {|let f(x: int) -> string {
        match x {
          | 0 -> "zero"
          | 1 -> "one"
          | _ -> "many"
        }
      }|}

let tc_match_guard_uses_bound_var ctxt =
  assert_ok ~ctxt
    {|let f(xs: int list) -> int {
        match xs {
          | x :: _ if x > 0 -> x
          | _ -> 0
        }
      }|}

let tc_match_guard_wrong_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: int) -> int {
        match x {
          | n if n + 1 -> n
          | _ -> 0
        }
      }|}

let tc_match_scrutinee_is_function ctxt =
  assert_error ~ctxt ~code:(Some E_Type_PatternTypeMismatch)
    {|let id(x: int) -> int { x }
      let f -> int {
        match id {
          | 0 -> 0
          | _ -> 1
        }
      }|}

let tc_match_branch_type_all_must_match ctxt =
  assert_error ~ctxt ~code:(Some E_Type_MatchBranchMismatch)
    {|let f(x: int) -> int {
        match x {
          | 0 -> 1
          | 1 -> "one"
          | _ -> 2
        }
      }|}

let tc_ho_wrong_function_arg_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      let to_str(x: string) -> string { x }
      apply(to_str, 1)|}

let tc_ho_function_arity_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      let add(x: int, y: int) -> int { x + y }
      apply(add, 1)|}

let tc_ho_return_function_called ctxt =
  assert_ok ~ctxt
    {|let make_adder(n: int) -> (int) -> int {
        let add(x: int) -> int { n + x }
        add
      }
      let add5 = make_adder(5)
      add5(3)|}

let tc_ho_return_function_wrong_call ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let make_adder(n: int) -> (int) -> int {
        let add(x: int) -> int { n + x }
        add
      }
      let add5 = make_adder(5)
      add5("not an int")|}

let tc_ho_pass_recursive_function ctxt =
  assert_ok ~ctxt
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      let double(x: int) -> int { x * 2 }
      apply(double, apply(double, 3))|}

let tc_ho_function_as_return_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f -> (int) -> int {
        let g(x: string) -> string { x }
        g
      }|}

let tc_ho_nested_higher_order ctxt =
  assert_ok ~ctxt
    {|let apply_twice(f: (int) -> int, x: int) -> int { f(f(x)) }
      let inc(x: int) -> int { x + 1 }
      apply_twice(inc, 0)|}

let tc_ho_function_stored_in_let ctxt =
  assert_ok ~ctxt
    {|let double(x: int) -> int { x * 2 }
      let f = double
      f(21)|}

let tc_ho_non_function_called_as_higher_order ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      apply(42, 1)|}

let suite =
  "typechecker tests"
  >::: [
         "[tc] int literal" >:: tc_test_int_literal;
         "[tc] float literal" >:: tc_test_float_literal;
         "[tc] string literal" >:: tc_test_string_literal;
         "[tc] bool literal" >:: tc_test_bool_literal;
         "[tc] unit literal" >:: tc_test_unit_literal;
         "[tc] let int" >:: tc_test_let_int;
         "[tc] let float" >:: tc_test_let_float;
         "[tc] let string" >:: tc_test_let_string;
         "[tc] let bool" >:: tc_test_let_bool;
         "[tc] let annotation mismatch" >:: tc_test_let_annotation_mismatch;
         "[tc] let annotation mismatch string"
         >:: tc_test_let_annotation_mismatch_string;
         "[tc] let no annotation" >:: tc_test_let_no_annotation;
         "[tc] let duplicate top level" >:: tc_test_let_duplicate_top_level;
         "[tc] let duplicate in block" >:: tc_test_let_duplicate_in_block;
         "[tc] let param duplicate" >:: tc_test_let_param_duplicate;
         "[tc] undefined variable" >:: tc_test_undefined_variable;
         "[tc] variable in scope" >:: tc_test_variable_in_scope;
         "[tc] variable used before definition"
         >:: tc_test_variable_used_before_definition;
         "[tc] simple function" >:: tc_test_simple_function;
         "[tc] function return mismatch" >:: tc_test_function_return_mismatch;
         "[tc] function missing annotation"
         >:: tc_test_function_missing_annotation;
         "[tc] function no return annotation"
         >:: tc_test_function_no_return_annotation;
         "[tc] multi param function" >:: tc_test_multi_param_function;
         "[tc] function returning bool" >:: tc_test_function_returning_bool;
         "[tc] function returning float" >:: tc_test_function_returning_float;
         "[tc] function returning string" >:: tc_test_function_returning_string;
         "[tc] simple application" >:: tc_test_simple_application;
         "[tc] arity mismatch too few" >:: tc_test_arity_mismatch_too_few;
         "[tc] arity mismatch too many" >:: tc_test_arity_mismatch_too_many;
         "[tc] apply wrong arg type" >:: tc_test_apply_wrong_arg_type;
         "[tc] apply not a function" >:: tc_test_apply_not_a_function;
         "[tc] apply zero args ok" >:: tc_test_apply_zero_args_ok;
         "[tc] int add" >:: tc_test_int_add;
         "[tc] float add" >:: tc_test_float_add;
         "[tc] int float mismatch" >:: tc_test_int_float_mismatch;
         "[tc] float int mismatch" >:: tc_test_float_int_mismatch;
         "[tc] int sub" >:: tc_test_int_sub;
         "[tc] int mul" >:: tc_test_int_mul;
         "[tc] int div" >:: tc_test_int_div;
         "[tc] float mul" >:: tc_test_float_mul;
         "[tc] string add invalid" >:: tc_test_string_add_invalid;
         "[tc] negate int" >:: tc_test_negate_int;
         "[tc] negate float" >:: tc_test_negate_float;
         "[tc] negate bool invalid" >:: tc_test_negate_bool_invalid;
         "[tc] int less" >:: tc_test_int_less;
         "[tc] int greater" >:: tc_test_int_greater;
         "[tc] float less" >:: tc_test_float_less;
         "[tc] int equal" >:: tc_test_int_equal;
         "[tc] int not equal" >:: tc_test_int_not_equal;
         "[tc] bool equal" >:: tc_test_bool_equal;
         "[tc] string equal" >:: tc_test_string_equal;
         "[tc] compare type mismatch" >:: tc_test_compare_type_mismatch;
         "[tc] equal type mismatch" >:: tc_test_equal_type_mismatch;
         "[tc] simple if" >:: tc_test_simple_if;
         "[tc] if with condition" >:: tc_test_if_with_condition;
         "[tc] if branch type mismatch" >:: tc_test_if_branch_type_mismatch;
         "[tc] if condition not bool" >:: tc_test_if_condition_not_bool;
         "[tc] if missing else" >:: tc_test_if_missing_else;
         "[tc] if float branches" >:: tc_test_if_float_branches;
         "[tc] if string branches" >:: tc_test_if_string_branches;
         "[tc] nested if" >:: tc_test_nested_if;
         "[tc] if in function" >:: tc_test_if_in_function;
         "[tc] match int" >:: tc_test_match_int;
         "[tc] match bool" >:: tc_test_match_bool;
         "[tc] match branch mismatch" >:: tc_test_match_branch_mismatch;
         "[tc] match pattern mismatch" >:: tc_test_match_pattern_mismatch;
         "[tc] match wildcard" >:: tc_test_match_wildcard;
         "[tc] match variable pattern" >:: tc_test_match_variable_pattern;
         "[tc] match variable used in body"
         >:: tc_test_match_variable_used_in_body;
         "[tc] match with guard" >:: tc_test_match_with_guard;
         "[tc] match guard not bool" >:: tc_test_match_guard_not_bool;
         "[tc] match nil pattern" >:: tc_test_match_nil_pattern;
         "[tc] match cons pattern" >:: tc_test_match_cons_pattern;
         "[tc] match string" >:: tc_test_match_string;
         "[tc] recursive function" >:: tc_test_recursive_function;
         "[tc] recursive missing return annotation"
         >:: tc_test_recursive_missing_return_annotation;
         "[tc] nested recursive function" >:: tc_test_nested_recursive_function;
         "[tc] block returns last" >:: tc_test_block_returns_last;
         "[tc] block let scoping" >:: tc_test_block_let_scoping;
         "[tc] block sequential lets" >:: tc_test_block_sequential_lets;
         "[tc] block let uses previous" >:: tc_test_block_let_uses_previous;
         "[tc] match nil" >:: tc_test_match_nil;
         "[tc] match cons head" >:: tc_test_match_cons_head;
         "[tc] match cons wrong type" >:: tc_test_match_cons_wrong_type;
         "[tc] function as arg" >:: tc_test_function_as_arg;
         "[tc] function return type" >:: tc_test_function_return_type;
         "[tc] primed name" >:: tc_test_primed_name;
         "[tc] double primed name" >:: tc_test_double_primed_name;
         "[tc] simple" >:: tc_test_adt_simple;
         "[tc] constructor with field" >:: tc_test_adt_constructor_with_field;
         "[tc] constructor wrong arity" >:: tc_test_adt_constructor_wrong_arity;
         "[tc] constructor wrong field type"
         >:: tc_test_adt_constructor_wrong_field_type;
         "[tc] match constructor" >:: tc_test_adt_match_constructor;
         "[tc] match branch mismatch" >:: tc_test_adt_match_branch_mismatch;
         "[tc] match pattern type mismatch"
         >:: tc_test_adt_match_pattern_type_mismatch;
         "[tc] nested constructor" >:: tc_test_adt_nested_constructor;
         "[tc] result type" >:: tc_test_adt_result_type;
         "[tc] recursive match" >:: tc_test_adt_recursive_match;
         "[tc] multi field pattern" >:: tc_test_adt_multi_field_pattern;
         "[tc] undefined constructor" >:: tc_test_adt_undefined_constructor;
         "[tc] cons int list" >:: tc_cons_int_list;
         "[tc] cons onto empty" >:: tc_cons_onto_empty;
         "[tc] cons variable onto list" >:: tc_cons_variable_onto_list;
         "[tc] cons wrong element type" >:: tc_cons_wrong_element_type;
         "[tc] cons wrong tail type" >:: tc_cons_wrong_tail_type;
         "[tc] cons nested list" >:: tc_cons_nested_list;
         "[tc] cons in match body" >:: tc_cons_in_match_body;
         "[tc] list type annotation accepted"
         >:: tc_list_type_annotation_accepted;
         "[tc] list type float accepted" >:: tc_list_type_float_accepted;
         "[tc] list type mismatch cons head" >:: tc_list_type_mismatch_cons_head;
         "[tc] list passed to function" >:: tc_list_passed_to_function;
         "[tc] adt three variants" >:: tc_adt_three_variants;
         "[tc] adt three field constructor" >:: tc_adt_three_field_constructor;
         "[tc] adt three field wrong arity" >:: tc_adt_three_field_wrong_arity;
         "[tc] adt nested match" >:: tc_adt_nested_match;
         "[tc] adt constructor in branch" >:: tc_adt_constructor_in_branch;
         "[tc] adt field type mismatch" >:: tc_adt_field_type_mismatch;
         "[tc] adt match binds fields" >:: tc_adt_match_binds_fields;
         "[tc] adt match wrong field count" >:: tc_adt_match_wrong_field_count;
         "[tc] adt recursive type" >:: tc_adt_recursive_type;
         "[tc] adt mutually used in function"
         >:: tc_adt_mutually_used_in_function;
         "[tc] if missing else in block" >:: tc_if_missing_else_in_block;
         "[tc] if missing else nested" >:: tc_if_missing_else_nested;
         "[tc] if condition is function" >:: tc_if_condition_is_function;
         "[tc] if condition is list" >:: tc_if_condition_is_list;
         "[tc] if both branches unit" >:: tc_if_both_branches_unit;
         "[tc] match all branches same type" >:: tc_match_all_branches_same_type;
         "[tc] match guard uses bound var" >:: tc_match_guard_uses_bound_var;
         "[tc] match guard wrong type" >:: tc_match_guard_wrong_type;
         "[tc] match scrutinee is function" >:: tc_match_scrutinee_is_function;
         "[tc] match branch type all must match"
         >:: tc_match_branch_type_all_must_match;
         "[tc] ho wrong function arg type" >:: tc_ho_wrong_function_arg_type;
         "[tc] ho function arity mismatch" >:: tc_ho_function_arity_mismatch;
         "[tc] ho return function called" >:: tc_ho_return_function_called;
         "[tc] ho return function wrong call"
         >:: tc_ho_return_function_wrong_call;
         "[tc] ho pass recursive function" >:: tc_ho_pass_recursive_function;
         "[tc] ho function as return type mismatch"
         >:: tc_ho_function_as_return_type_mismatch;
         "[tc] ho nested higher order" >:: tc_ho_nested_higher_order;
         "[tc] ho function stored in let" >:: tc_ho_function_stored_in_let;
         "[tc] ho non function called as higher order"
         >:: tc_ho_non_function_called_as_higher_order;
       ]
