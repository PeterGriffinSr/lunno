open OUnit2
open Lunno_common
open Test_helpers

let tc_poly_identity_int ctxt = assert_ok ~ctxt {|let id(x) { x }
      id(42)|}

let tc_poly_identity_float ctxt =
  assert_ok ~ctxt {|let id(x) { x }
      id(3.14)|}

let tc_poly_identity_string ctxt =
  assert_ok ~ctxt {|let id(x) { x }
      id("hello")|}

let tc_poly_identity_bool ctxt =
  assert_ok ~ctxt {|let id(x) { x }
      id(true)|}

let tc_poly_identity_two_types ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let a: int    = id(1)
      let b: string = id("hi")
      a|}

let tc_poly_identity_three_types ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let a: int    = id(0)
      let b: float  = id(0.0)
      let c: bool   = id(false)
      a|}

let tc_poly_const_int_string ctxt =
  assert_ok ~ctxt
    {|let k(x, y) { x }
      let r: int = k(42, "ignored")
      r|}

let tc_poly_const_string_int ctxt =
  assert_ok ~ctxt
    {|let k(x, y) { x }
      let r: string = k("hello", 99)
      r|}

let tc_poly_const_bool_float ctxt =
  assert_ok ~ctxt
    {|let k(x, y) { x }
      let r: bool = k(true, 2.71)
      r|}

let tc_poly_const_multiple_instantiations ctxt =
  assert_ok ~ctxt
    {|let k(x, y) { x }
      let a: int    = k(1,    "a")
      let b: string = k("b",  2)
      let c: bool   = k(true, false)
      a|}

let tc_poly_apply_int ctxt =
  assert_ok ~ctxt
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      let double(x: int) -> int { x * 2 }
      apply(double, 5)|}

let tc_poly_apply_to_string_fn ctxt =
  assert_ok ~ctxt
    {|let apply(f: (string) -> int, s: string) -> int { f(s) }
      let length_proxy(s: string) -> int { 0 }
      apply(length_proxy, "hi")|}

let tc_poly_twice ctxt =
  assert_ok ~ctxt
    {|let twice(f: (int) -> int, x: int) -> int { f(f(x)) }
      let succ(n: int) -> int { n + 1 }
      twice(succ, 0)|}

let tc_poly_length_int_list ctxt =
  assert_ok ~ctxt
    {|let length(xs: int list) -> int {
        match xs {
          | []      -> 0
          | _ :: t  -> 1 + length(t)
        }
      }
      length(1 :: 2 :: 3 :: [])|}

let tc_poly_length_no_annotations ctxt =
  assert_ok ~ctxt
    {|let length(xs) {
        match xs {
          | []      -> 0
          | _ :: t  -> 1 + length(t)
        }
      }|}

let tc_poly_length_untyped_param ctxt =
  assert_ok ~ctxt
    {|let length(xs) -> int {
        match xs {
          | []      -> 0
          | _ :: t  -> 1 + length(t)
        }
      }|}

let tc_poly_fib_no_annotations ctxt =
  assert_ok ~ctxt
    {|let fib(n) {
        if n < 2 then n else fib(n - 1) + fib(n - 2)
      }|}

let tc_poly_length_untyped_called ctxt =
  assert_ok ~ctxt
    {|let length(xs) -> int {
        match xs {
          | []      -> 0
          | _ :: t  -> 1 + length(t)
        }
      }
      length([1, 2, 3])|}

let tc_poly_fib_no_annotations_called ctxt =
  assert_ok ~ctxt
    {|let fib(n) {
        if n < 2 then n else fib(n - 1) + fib(n - 2)
      }
      fib(10)|}

let tc_poly_max_no_annotations ctxt =
  assert_ok ~ctxt
    {|let max(a, b) {
        if a > b then a else b
      }
      max(3, 5)|}

let tc_poly_length_untyped ctxt =
  assert_ok ~ctxt
    {|let length(xs) -> int {
        match xs {
          | []      -> 0
          | _ :: t  -> 1 + length(t)
        }
      }|}

let tc_poly_head_int ctxt =
  assert_ok ~ctxt
    {|let head(xs: int list) -> int {
        match xs {
          | x :: _ -> x
          | [] -> 0
        }
      }
      head(7 :: [])|}

let tc_poly_list_untyped_param ctxt =
  assert_ok ~ctxt
    {|let is_empty(xs) -> bool {
        match xs {
          | []    -> true
          | _ :: _ -> false
        }
      }|}

let tc_poly_let_generalization_in_block ctxt =
  assert_ok ~ctxt
    {|let f -> int {
        let id(x) { x }
        let a: int    = id(1)
        let b: string = id("two")
        a
      }|}

let tc_poly_block_let_multi_use ctxt =
  assert_ok ~ctxt
    {|let test -> bool {
        let id(x) { x }
        let n: int  = id(42)
        let s: bool = id(true)
        s
      }|}

let tc_poly_two_poly_lets_in_block ctxt =
  assert_ok ~ctxt
    {|let outer -> int {
        let id(x)    { x    }
        let const(x, y) { x }
        let a: int    = id(1)
        let b: string = id("hi")
        let c: int    = const(10, "drop")
        let d: bool   = const(false, 99)
        a
      }|}

let tc_poly_fib_recursive ctxt =
  assert_ok ~ctxt
    {|let fib(n: int) -> int {
        match n {
          | 0 -> 0
          | 1 -> 1
          | m -> fib(m - 1) + fib(m - 2)
        }
      }|}

let tc_poly_shared_poly_helper ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let foo(n: int)    -> int    { id(n) }
      let bar(s: string) -> string { id(s) }
      foo(1)|}

let tc_poly_returns_function ctxt =
  assert_ok ~ctxt
    {|let add_n(n: int) -> (int) -> int {
        let adder(x: int) -> int { n + x }
        adder
      }
      let add5 = add_n(5)
      add5(10)|}

let tc_poly_higher_order_returns ctxt =
  assert_ok ~ctxt
    {|let compose(f: (int) -> int, g: (int) -> int) -> (int) -> int {
        let h(x: int) -> int { f(g(x)) }
        h
      }
      let inc(x: int) -> int { x + 1 }
      let dbl(x: int) -> int { x * 2 }
      let inc_then_dbl = compose(dbl, inc)
      inc_then_dbl(3)|}

let tc_poly_match_variable_bind ctxt =
  assert_ok ~ctxt
    {|let first_or_default(xs: int list, default: int) -> int {
        match xs {
          | h :: _ -> h
          | []     -> default
        }
      }|}

let tc_poly_match_guard_poly ctxt =
  assert_ok ~ctxt
    {|let safe_head(xs: int list) -> int {
        match xs {
          | h :: _ if h > 0 -> h
          | _ -> 0
        }
      }|}

let tc_poly_identity_wrong_annotation ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let id(x) { x }
      let r: int = id("oops")|}

let tc_poly_apply_wrong_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let id_int(x: int) -> int { x }
      id_int("not an int")|}

let tc_poly_const_annotation_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let k(x, y) { x }
      let r: int = k("wrong", 42)|}

let tc_poly_recursive_mono ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: int) -> int { f(x) }
      f("oops")|}

let tc_poly_primed_name ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let id'(x) { x }
      let a: int    = id(1)
      let b: string = id'("hi")
      a|}

let tc_poly_outer_poly_inner_mono ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let f -> int {
        let id_int(x: int) -> int { x }
        id_int(99)
      }
      let g: string = id("still works outside")
      f()|}

let tc_poly_no_shadowing_error ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined)
    {|let id(x) { x }
      let f -> int {
        let id(x: int) -> int { x }
        id(99)
      }
      f()|}

let tc_poly_stress_many_instantiations ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let a: int    = id(0)
      let b: float  = id(0.0)
      let c: string = id("hello")
      let d: bool   = id(true)
      let e: int    = id(a)
      let f: float  = id(b)
      let g: string = id(c)
      let h: bool   = id(d)
      a|}

let tc_poly_stress_const ctxt =
  assert_ok ~ctxt
    {|let k(x, y) { x }
      let a: int    = k(1,     2.0)
      let b: float  = k(2.0,   "x")
      let c: string = k("hi",  true)
      let d: bool   = k(false, 0)
      let e: int    = k(a,     b)
      a|}

let tc_poly_list_literal_empty ctxt =
  assert_ok ~ctxt {|let xs: int list = []
      xs|}

let tc_poly_list_literal_singleton ctxt =
  assert_ok ~ctxt {|let xs: int list = [42]
      xs|}

let tc_poly_list_literal_multi ctxt =
  assert_ok ~ctxt {|let xs: int list = [1, 2, 3]
      xs|}

let tc_poly_list_literal_float ctxt =
  assert_ok ~ctxt {|let xs: float list = [1.0, 2.0, 3.0]
      xs|}

let tc_poly_list_literal_bool ctxt =
  assert_ok ~ctxt {|let xs: bool list = [true, false, true]
      xs|}

let tc_poly_list_literal_string ctxt =
  assert_ok ~ctxt {|let xs: string list = ["foo", "bar", "baz"]
      xs|}

let tc_poly_list_literal_as_arg ctxt =
  assert_ok ~ctxt
    {|let head(xs: int list) -> int {
        match xs { | x :: _ -> x | [] -> 0 }
      }
      head([10, 20, 30])|}

let tc_poly_list_literal_cons_prefix ctxt =
  assert_ok ~ctxt {|let xs: int list = 0 :: [1, 2, 3]
      xs|}

let tc_poly_list_literal_in_match ctxt =
  assert_ok ~ctxt
    {|let f(xs: int list) -> int {
        match xs {
          | []      -> 0
          | x :: _  -> x
        }
      }
      f([99])|}

let tc_poly_list_literal_empty_inferred ctxt =
  assert_ok ~ctxt
    {|let f(xs: int list) -> int {
        match xs { | [] -> 0 | x :: _ -> x }
      }
      f([])|}

let tc_poly_list_literal_length ctxt =
  assert_ok ~ctxt
    {|let length(xs: int list) -> int {
        match xs { | [] -> 0 | _ :: t -> 1 + length(t) }
      }
      length([1, 2, 3, 4, 5])|}

let tc_poly_list_literal_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let xs: int list = [1, 2.0]|}

let tc_poly_list_literal_annotation_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let xs: string list = [1, 2, 3]|}

let tc_poly_map_int ctxt =
  assert_ok ~ctxt
    {|let map_int(f: (int) -> int, xs: int list) -> int list {
        match xs {
          | []      -> []
          | h :: t  -> f(h) :: map_int(f, t)
        }
      }
      let double(x: int) -> int { x * 2 }
      map_int(double, [1, 2, 3])|}

let tc_poly_filter_int ctxt =
  assert_ok ~ctxt
    {|let filter(pred: (int) -> bool, xs: int list) -> int list {
        match xs {
          | []     -> []
          | h :: t ->
              if pred(h) then h :: filter(pred, t)
              else filter(pred, t)
        }
      }
      let is_pos(x: int) -> bool { x > 0 }
      filter(is_pos, [1, 0, 2, 0, 3])|}

let tc_poly_fold_left_int ctxt =
  assert_ok ~ctxt
    {|let fold_left(f: (int, int) -> int, acc: int, xs: int list) -> int {
        match xs {
          | []     -> acc
          | h :: t -> fold_left(f, f(acc, h), t)
        }
      }
      let add(a: int, b: int) -> int { a + b }
      fold_left(add, 0, [1, 2, 3, 4, 5])|}

let tc_poly_fold_right_int ctxt =
  assert_ok ~ctxt
    {|let fold_right(f: (int, int) -> int, xs: int list, acc: int) -> int {
        match xs {
          | []     -> acc
          | h :: t -> f(h, fold_right(f, t, acc))
        }
      }
      let add(a: int, b: int) -> int { a + b }
      fold_right(add, [1, 2, 3], 0)|}

let tc_poly_for_all_int ctxt =
  assert_ok ~ctxt
    {|let for_all(pred: (int) -> bool, xs: int list) -> bool {
        match xs {
          | []     -> true
          | h :: t -> if pred(h) then for_all(pred, t) else false
        }
      }
      let is_pos(x: int) -> bool { x > 0 }
      for_all(is_pos, [1, 2, 3])|}

let tc_poly_exists_int ctxt =
  assert_ok ~ctxt
    {|let exists(pred: (int) -> bool, xs: int list) -> bool {
        match xs {
          | []     -> false
          | h :: t -> if pred(h) then true else exists(pred, t)
        }
      }
      let is_neg(x: int) -> bool { x < 0 }
      exists(is_neg, [1, 2, 3])|}

let tc_poly_sum_via_fold ctxt =
  assert_ok ~ctxt
    {|let fold_left(f: (int, int) -> int, acc: int, xs: int list) -> int {
        match xs {
          | []     -> acc
          | h :: t -> fold_left(f, f(acc, h), t)
        }
      }
      let add(a: int, b: int) -> int { a + b }
      let sum(xs: int list) -> int { fold_left(add, 0, xs) }
      sum([10, 20, 30])|}

let tc_poly_reverse_int ctxt =
  assert_ok ~ctxt
    {|let reverse(xs: int list) -> int list {
        let rev_aux(acc: int list, ys: int list) -> int list {
          match ys {
            | []     -> acc
            | h :: t -> rev_aux(h :: acc, t)
          }
        }
        rev_aux([], xs)
      }
      reverse([1, 2, 3])|}

let tc_poly_zip_int ctxt =
  assert_ok ~ctxt
    {|data Pair = { | Pair(int, int) }
      let zip(xs: int list, ys: int list) -> Pair list {
        match xs {
          | []      -> []
          | x :: xt ->
              match ys {
                | []      -> []
                | y :: yt -> Pair(x, y) :: zip(xt, yt)
              }
        }
      }
      zip([1, 2, 3], [4, 5, 6])|}

let tc_poly_map_wrong_fn_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let map_int(f: (int) -> int, xs: int list) -> int list {
        match xs {
          | []     -> []
          | h :: t -> f(h) :: map_int(f, t)
        }
      }
      let to_str(x: string) -> string { x }
      map_int(to_str, [1, 2, 3])|}

let tc_poly_fold_wrong_accumulator_type ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let fold_left(f: (int, int) -> int, acc: int, xs: int list) -> int {
        match xs {
          | []     -> acc
          | h :: t -> fold_left(f, f(acc, h), t)
        }
      }
      let add(a: int, b: int) -> int { a + b }
      fold_left(add, "wrong", [1, 2, 3])|}

let tc_poly_identity_applied_to_wrong_type_after_constraint ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: int) -> int {
        let id(y) { y }
        let a: int = id(x)
        let b: int = id("oops")
        a + b
      }|}

let tc_poly_const_second_arg_constrains_return ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let k(x, y) { x }
      let r: int = k("not int", 42)|}

let tc_poly_recursive_arg_type_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let length(xs: int list) -> int {
        match xs { | [] -> 0 | _ :: t -> 1 + length(t) }
      }
      length(["a", "b"])|}

let tc_poly_apply_result_annotation_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let id(x) { x }
      let r: float = id(42)|}

let tc_poly_twice_wrong_fn ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let twice(f: (int) -> int, x: int) -> int { f(f(x)) }
      let to_bool(x: int) -> bool { x > 0 }
      twice(to_bool, 1)|}

let tc_poly_higher_order_arg_arity_wrong ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let apply(f: (int) -> int, x: int) -> int { f(x) }
      let add(a: int, b: int) -> int { a + b }
      apply(add, 1)|}

let tc_poly_adt_option_wrap_unwrap ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let wrap(x: int) -> Option { Some(x) }
      let unwrap(o: Option) -> int {
        match o { | Some(n) -> n | None -> 0 }
      }
      let id(x) { x }
      let o: Option = id(wrap(42))
      unwrap(o)|}

let tc_poly_adt_constructor_in_poly_context ctxt =
  assert_ok ~ctxt
    {|data Box = { | Box(int) }
      let id(x) { x }
      let b: Box = id(Box(99))
      b|}

let tc_poly_adt_const_drops_adt ctxt =
  assert_ok ~ctxt
    {|data Color = { | Red | Green | Blue }
      let k(x, y) { x }
      let c: Color = k(Red, 42)
      c|}

let tc_poly_adt_id_used_with_adt_and_int ctxt =
  assert_ok ~ctxt
    {|data Flag = { | On | Off }
      let id(x) { x }
      let f: Flag = id(On)
      let n: int  = id(0)
      n|}

let tc_poly_adt_match_result_via_id ctxt =
  assert_ok ~ctxt
    {|data Option = { | Some(int) | None }
      let id(x) { x }
      let f(o: Option) -> int {
        let o2: Option = id(o)
        match o2 { | Some(n) -> n | None -> 0 }
      }
      f(Some(7))|}

let tc_poly_adt_wrong_type_to_poly ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|data Color = { | Red | Green | Blue }
      let id(x) { x }
      let c: Color = id(Red)
      let n: int   = id(c)
      n|}

let tc_poly_adt_list_of_adts ctxt =
  assert_ok ~ctxt
    {|data Color = { | Red | Green | Blue }
      let to_int(c: Color) -> int {
        match c { | Red -> 0 | Green -> 1 | Blue -> 2 }
      }
      let fold_left(f: (int, Color) -> int, acc: int, xs: Color list) -> int {
        match xs { | [] -> acc | h :: t -> fold_left(f, f(acc, h), t) }
      }
      let combine(acc: int, c: Color) -> int { acc + to_int(c) }
      fold_left(combine, 0, [Red, Green, Blue])|}

let tc_poly_gen_inner_shadows_outer ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined)
    {|let id(x) { x }
      let f -> int {
        let id(x) { x }
        id(1)
      }
      f()|}

let tc_poly_gen_deep_nesting ctxt =
  assert_ok ~ctxt
    {|let outer -> int {
        let id(x) { x }
        let middle -> int {
          let a: int    = id(1)
          let b: string = id("hi")
          a
        }
        middle()
      }
      outer()|}

let tc_poly_gen_poly_in_recursive_fn ctxt =
  assert_ok ~ctxt
    {|let count(xs: int list) -> int {
        let id(x) { x }
        match xs {
          | []     -> id(0)
          | _ :: t -> id(1) + count(t)
        }
      }
      count([1, 2, 3])|}

let tc_poly_gen_let_used_across_branches ctxt =
  assert_ok ~ctxt
    {|let f(x: int) -> int {
        let id(y) { y }
        if x > 0 then id(x) else id(0)
      }|}

let tc_poly_gen_poly_helper_called_multiple_times ctxt =
  assert_ok ~ctxt
    {|let test -> int {
        let wrap(x) { x }
        let a: int    = wrap(1)
        let b: string = wrap("two")
        let c: bool   = wrap(true)
        let d: int    = wrap(a)
        d
      }|}

let tc_poly_gen_inner_mono_doesnt_escape ctxt =
  assert_ok ~ctxt
    {|let id(x) { x }
      let f -> int {
        let add_one(n: int) -> int { n + 1 }
        add_one(id(5))
      }
      let g: string = id("still polymorphic")
      f()|}

let tc_poly_gen_two_independent_poly_fns ctxt =
  assert_ok ~ctxt
    {|let id(x)    { x }
      let const(x, y) { x }
      let a: int    = id(const(1,    "drop"))
      let b: string = id(const("hi", 0))
      let c: bool   = const(id(true), id(0))
      a|}

let tc_poly_gen_scoped_let_not_visible_outside ctxt =
  assert_error ~ctxt ~code:(Some E_Type_UndefinedVariable)
    {|let f -> int {
        let helper(x: int) -> int { x + 1 }
        helper(0)
      }
      helper(1)|}

let tc_poly_gen_param_name_same_as_outer_let ctxt =
  assert_error ~ctxt ~code:(Some E_Type_AlreadyDefined)
    {|let x = 10
      let f(x: int) -> int { x + 1 }
      f(x)|}

let tc_poly_gen_nested_blocks_each_have_own_scope ctxt =
  assert_ok ~ctxt
    {|let outer -> int {
        let id(x) { x }
        let a: int = id(1)
        let inner -> string {
          let b: string = id("hi")
          b
        }
        a
      }
      outer()|}

let suite =
  "poly tests"
  >::: [
         "[poly] identity int" >:: tc_poly_identity_int;
         "[poly] identity float" >:: tc_poly_identity_float;
         "[poly] identity string" >:: tc_poly_identity_string;
         "[poly] identity bool" >:: tc_poly_identity_bool;
         "[poly] identity two types" >:: tc_poly_identity_two_types;
         "[poly] identity three types" >:: tc_poly_identity_three_types;
         "[poly] const int string" >:: tc_poly_const_int_string;
         "[poly] const string int" >:: tc_poly_const_string_int;
         "[poly] const bool float" >:: tc_poly_const_bool_float;
         "[poly] const multiple instantiations"
         >:: tc_poly_const_multiple_instantiations;
         "[poly] apply int" >:: tc_poly_apply_int;
         "[poly] apply to string fn" >:: tc_poly_apply_to_string_fn;
         "[poly] twice" >:: tc_poly_twice;
         "[poly] length int list" >:: tc_poly_length_int_list;
         "[poly] length no annotations" >:: tc_poly_length_no_annotations;
         "[poly] length untyped param" >:: tc_poly_length_untyped_param;
         "[poly] fib no annotations" >:: tc_poly_fib_no_annotations;
         "[poly] length untyped called" >:: tc_poly_length_untyped_called;
         "[poly] fib no annotations called"
         >:: tc_poly_fib_no_annotations_called;
         "[poly] max no annotations" >:: tc_poly_max_no_annotations;
         "[poly] head int" >:: tc_poly_head_int;
         "[poly] list untyped param" >:: tc_poly_list_untyped_param;
         "[poly] let generalization in block"
         >:: tc_poly_let_generalization_in_block;
         "[poly] block let multi use" >:: tc_poly_block_let_multi_use;
         "[poly] two poly lets in block" >:: tc_poly_two_poly_lets_in_block;
         "[poly] fib recursive" >:: tc_poly_fib_recursive;
         "[poly] shared poly helper" >:: tc_poly_shared_poly_helper;
         "[poly] returns function" >:: tc_poly_returns_function;
         "[poly] higher order returns" >:: tc_poly_higher_order_returns;
         "[poly] match variable bind" >:: tc_poly_match_variable_bind;
         "[poly] match guard poly" >:: tc_poly_match_guard_poly;
         "[poly] identity wrong annotation"
         >:: tc_poly_identity_wrong_annotation;
         "[poly] apply wrong type" >:: tc_poly_apply_wrong_type;
         "[poly] const annotation mismatch"
         >:: tc_poly_const_annotation_mismatch;
         "[poly] recursive mono" >:: tc_poly_recursive_mono;
         "[poly] primed name" >:: tc_poly_primed_name;
         "[poly] outer poly inner mono" >:: tc_poly_outer_poly_inner_mono;
         "[poly] no shadowing error" >:: tc_poly_no_shadowing_error;
         "[poly] stress many instantiations"
         >:: tc_poly_stress_many_instantiations;
         "[poly] stress const" >:: tc_poly_stress_const;
         "[poly] list literal empty" >:: tc_poly_list_literal_empty;
         "[poly] list literal singleton" >:: tc_poly_list_literal_singleton;
         "[poly] list literal multi" >:: tc_poly_list_literal_multi;
         "[poly] list literal float" >:: tc_poly_list_literal_float;
         "[poly] list literal bool" >:: tc_poly_list_literal_bool;
         "[poly] list literal string" >:: tc_poly_list_literal_string;
         "[poly] list literal as arg" >:: tc_poly_list_literal_as_arg;
         "[poly] list literal cons prefix" >:: tc_poly_list_literal_cons_prefix;
         "[poly] list literal in match" >:: tc_poly_list_literal_in_match;
         "[poly] list literal empty inferred"
         >:: tc_poly_list_literal_empty_inferred;
         "[poly] list literal length" >:: tc_poly_list_literal_length;
         "[poly] list literal type mismatch"
         >:: tc_poly_list_literal_type_mismatch;
         "[poly] list literal annotation mismatch"
         >:: tc_poly_list_literal_annotation_mismatch;
         "[poly] map int" >:: tc_poly_map_int;
         "[poly] filter int" >:: tc_poly_filter_int;
         "[poly] fold left int" >:: tc_poly_fold_left_int;
         "[poly] fold right int" >:: tc_poly_fold_right_int;
         "[poly] for all int" >:: tc_poly_for_all_int;
         "[poly] exists int" >:: tc_poly_exists_int;
         "[poly] sum via fold" >:: tc_poly_sum_via_fold;
         "[poly] reverse int" >:: tc_poly_reverse_int;
         "[poly] zip int" >:: tc_poly_zip_int;
         "[poly] map wrong fn type" >:: tc_poly_map_wrong_fn_type;
         "[poly] fold wrong accumulator type"
         >:: tc_poly_fold_wrong_accumulator_type;
         "[poly] identity applied to wrong type after constraint"
         >:: tc_poly_identity_applied_to_wrong_type_after_constraint;
         "[poly] const second arg constrains return"
         >:: tc_poly_const_second_arg_constrains_return;
         "[poly] recursive arg type mismatch"
         >:: tc_poly_recursive_arg_type_mismatch;
         "[poly] apply result annotation mismatch"
         >:: tc_poly_apply_result_annotation_mismatch;
         "[poly] twice wrong fn" >:: tc_poly_twice_wrong_fn;
         "[poly] higher order arg arity wrong"
         >:: tc_poly_higher_order_arg_arity_wrong;
         "[poly] adt option wrap unwrap" >:: tc_poly_adt_option_wrap_unwrap;
         "[poly] adt constructor in poly context"
         >:: tc_poly_adt_constructor_in_poly_context;
         "[poly] adt const drops adt" >:: tc_poly_adt_const_drops_adt;
         "[poly] adt id used with adt and int"
         >:: tc_poly_adt_id_used_with_adt_and_int;
         "[poly] adt match result via id" >:: tc_poly_adt_match_result_via_id;
         "[poly] adt wrong type to poly" >:: tc_poly_adt_wrong_type_to_poly;
         "[poly] adt list of adts" >:: tc_poly_adt_list_of_adts;
         "[poly] gen inner shadows outer" >:: tc_poly_gen_inner_shadows_outer;
         "[poly] gen deep nesting" >:: tc_poly_gen_deep_nesting;
         "[poly] gen poly in recursive fn" >:: tc_poly_gen_poly_in_recursive_fn;
         "[poly] gen let used across branches"
         >:: tc_poly_gen_let_used_across_branches;
         "[poly] gen poly helper called multiple times"
         >:: tc_poly_gen_poly_helper_called_multiple_times;
         "[poly] gen inner mono doesnt escape"
         >:: tc_poly_gen_inner_mono_doesnt_escape;
         "[poly] gen two independent poly fns"
         >:: tc_poly_gen_two_independent_poly_fns;
         "[poly] gen scoped let not visible outside"
         >:: tc_poly_gen_scoped_let_not_visible_outside;
         "[poly] gen param name same as outer let"
         >:: tc_poly_gen_param_name_same_as_outer_let;
         "[poly] gen nested blocks each have own scope"
         >:: tc_poly_gen_nested_blocks_each_have_own_scope;
       ]
