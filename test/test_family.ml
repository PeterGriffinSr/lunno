open OUnit2
open Lunno_common
open Test_helpers

let tc_family_int_ops ctxt =
  assert_ok ~ctxt {|let f(int[a, b]) -> int { a + b }
      f(1, 2)|}

let tc_family_float_ops ctxt =
  assert_ok ~ctxt {|let f(float[a, b]) -> float { a + b }
      f(1.0, 2.0)|}

let tc_family_int_rejects_float ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[a, b]) -> int { a + b }
      f(1.0, 2.0)|}

let tc_family_float_rejects_int ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(float[a, b]) -> float { a + b }
      f(1, 2)|}

let tc_family_int_comparison ctxt =
  assert_ok ~ctxt {|let is_pos(int[n]) -> bool { n > 0 }
      is_pos(5)|}

let tc_family_float_comparison ctxt =
  assert_ok ~ctxt {|let is_pos(float[n]) -> bool { n > 0.0 }
      is_pos(1.5)|}

let tc_family_mixed_int_float_rejected ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[a]) -> int { a + 1 }
      f(3.14)|}

let tc_family_uniform_three_int ctxt =
  assert_ok ~ctxt
    {|let sum3(int[a, b, c]) -> int { a + b + c }
      sum3(1, 2, 3)|}

let tc_family_uniform_three_float ctxt =
  assert_ok ~ctxt
    {|let sum3(float[a, b, c]) -> float { a + b + c }
      sum3(1.0, 2.0, 3.0)|}

let tc_family_return_annotation_int ctxt =
  assert_ok ~ctxt {|let double(int[x]) -> int { x + x }
      double(21)|}

let tc_family_return_annotation_float ctxt =
  assert_ok ~ctxt {|let double(float[x]) -> float { x + x }
      double(1.5)|}

let tc_family_mixed_uniform_and_typed ctxt =
  assert_ok ~ctxt
    {|let f(int[a, b], s: string) -> int { a + b }
      f(1, 2, "ignored")|}

let tc_family_int_i32 ctxt =
  assert_ok ~ctxt {|let f(x: i32) -> i32 { x + x }
      f(10)|}

let tc_family_int_i64 ctxt =
  assert_ok ~ctxt {|let f(x: i64) -> i64 { x + x }
      f(100)|}

let tc_family_float_f32 ctxt =
  assert_ok ~ctxt {|let f(x: f32) -> f32 { x + x }
      f(1.0)|}

let tc_family_float_f64 ctxt =
  assert_ok ~ctxt {|let f(x: f64) -> f64 { x + x }
      f(1.0)|}

let tc_family_int_subtypes_no_mix ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i32, y: i64) -> i64 { x + y }|}

let tc_family_in_recursive_function ctxt =
  assert_ok ~ctxt
    {|let sum(int[n]) -> int {
        if n < 1 then 0 else n + sum(n - 1)
      }
      sum(10)|}

let tc_family_in_match ctxt =
  assert_ok ~ctxt
    {|let classify(int[n]) -> string {
        match n {
          | 0 -> "zero"
          | _ -> "nonzero"
        }
      }|}

let tc_family_poly_with_family ctxt =
  assert_ok ~ctxt
    {|let add(int[a, b]) -> int { a + b }
      let x: int = add(1, 2)
      x|}

let tc_family_i8_basic ctxt =
  assert_ok ~ctxt {|let f(x: i8) -> i8 { x + x }
      f(10)|}

let tc_family_i16_basic ctxt =
  assert_ok ~ctxt {|let f(x: i16) -> i16 { x + x }
      f(100)|}

let tc_family_i8_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: i8, y: i8) -> i8 { x + y }
      f(1, 2)|}

let tc_family_i16_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: i16, y: i16) -> i16 { x - y }
      f(10, 3)|}

let tc_family_i32_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: i32, y: i32) -> i32 { x * y }
      f(6, 7)|}

let tc_family_i64_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: i64, y: i64) -> i64 { x / y }
      f(100, 5)|}

let tc_family_i8_comparison ctxt =
  assert_ok ~ctxt {|let f(x: i8) -> bool { x > 0 }
      f(1)|}

let tc_family_i16_comparison ctxt =
  assert_ok ~ctxt {|let f(x: i16) -> bool { x < 1000 }
      f(500)|}

let tc_family_i32_comparison ctxt =
  assert_ok ~ctxt {|let f(x: i32) -> bool { x = 0 }
      f(0)|}

let tc_family_i64_comparison ctxt =
  assert_ok ~ctxt {|let f(x: i64) -> bool { x <> 0 }
      f(1)|}

let tc_family_i8_rejects_i16 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i8, y: i16) -> i8 { x + y }|}

let tc_family_i16_rejects_i32 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i16, y: i32) -> i16 { x + y }|}

let tc_family_i32_rejects_i64 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i32, y: i64) -> i32 { x + y }|}

let tc_family_i8_rejects_float ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i8) -> i8 { x }
      f(1.0)|}

let tc_family_i64_rejects_float ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: i64) -> i64 { x }
      f(1.0)|}

let tc_family_f32_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: f32, y: f32) -> f32 { x + y }
      f(1.0, 2.0)|}

let tc_family_f64_arithmetic ctxt =
  assert_ok ~ctxt {|let f(x: f64, y: f64) -> f64 { x * y }
      f(2.0, 3.0)|}

let tc_family_f32_comparison ctxt =
  assert_ok ~ctxt {|let f(x: f32) -> bool { x > 0.0 }
      f(1.0)|}

let tc_family_f64_comparison ctxt =
  assert_ok ~ctxt {|let f(x: f64) -> bool { x < 100.0 }
      f(3.14)|}

let tc_family_f32_rejects_f64 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: f32, y: f64) -> f32 { x + y }|}

let tc_family_f64_rejects_f32 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: f64, y: f32) -> f64 { x + y }|}

let tc_family_f32_rejects_int ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: f32) -> f32 { x }
      f(1)|}

let tc_family_f64_rejects_int ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(x: f64) -> f64 { x }
      f(42)|}

let tc_family_int_accepts_i8 ctxt =
  assert_ok ~ctxt {|let f(int[x]) -> int { x + 0 }
      f(1)|}

let tc_family_int_accepts_i16 ctxt =
  assert_ok ~ctxt {|let f(int[x]) -> int { x + 0 }
      f(1)|}

let tc_family_int_accepts_i32 ctxt =
  assert_ok ~ctxt {|let f(int[x]) -> int { x + 0 }
      f(1)|}

let tc_family_int_accepts_i64 ctxt =
  assert_ok ~ctxt {|let f(int[x]) -> int { x + 0 }
      f(1)|}

let tc_family_int_rejects_f32 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[x]) -> int { x }
      f(1.0)|}

let tc_family_int_rejects_f64 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[x]) -> int { x }
      f(3.14)|}

let tc_family_int_uniform_four ctxt =
  assert_ok ~ctxt
    {|let sum4(int[a, b, c, d]) -> int { a + b + c + d }
      sum4(1, 2, 3, 4)|}

let tc_family_int_in_list ctxt =
  assert_ok ~ctxt {|let f(int[n]) -> int list { n :: [] }
      f(5)|}

let tc_family_float_accepts_f32 ctxt =
  assert_ok ~ctxt {|let f(float[x]) -> float { x + 0.0 }
      f(1.0)|}

let tc_family_float_accepts_f64 ctxt =
  assert_ok ~ctxt {|let f(float[x]) -> float { x + 0.0 }
      f(1.0)|}

let tc_family_float_rejects_i8 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(float[x]) -> float { x }
      f(1)|}

let tc_family_float_rejects_i32 ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(float[x]) -> float { x }
      f(42)|}

let tc_family_float_uniform_two ctxt =
  assert_ok ~ctxt
    {|let avg(float[a, b]) -> float { (a + b) / 2.0 }
      avg(1.0, 3.0)|}

let tc_family_int_with_string_param ctxt =
  assert_ok ~ctxt
    {|let f(int[a, b], label: string) -> int { a + b }
      f(3, 4, "sum")|}

let tc_family_float_with_bool_param ctxt =
  assert_ok ~ctxt
    {|let f(float[x], negate: bool) -> float {
        if negate then 0.0 - x else x
      }
      f(1.5, false)|}

let tc_family_int_with_list_param ctxt =
  assert_ok ~ctxt
    {|let prepend(int[x], xs: int list) -> int list { x :: xs }
      prepend(0, [1, 2, 3])|}

let tc_family_multiple_groups ctxt =
  assert_ok ~ctxt
    {|let dot(int[a, b], int[c, d]) -> int { a * c + b * d }
      dot(1, 2, 3, 4)|}

let tc_family_int_group_wrong_mixed_call ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[a, b]) -> int { a + b }
      f(1, 1.0)|}

let tc_family_int_in_if ctxt =
  assert_ok ~ctxt
    {|let abs(int[n]) -> int {
        if n < 0 then 0 - n else n
      }
      abs(5)|}

let tc_family_float_in_if ctxt =
  assert_ok ~ctxt
    {|let clamp(float[x]) -> float {
        if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x
      }
      clamp(0.5)|}

let tc_family_int_in_match_literal ctxt =
  assert_ok ~ctxt
    {|let name(int[n]) -> string {
        match n {
          | 0 -> "zero"
          | 1 -> "one"
          | _ -> "other"
        }
      }
      name(2)|}

let tc_family_float_in_match_wildcard ctxt =
  assert_ok ~ctxt
    {|let sign(float[x]) -> string {
        match x {
          | _ -> if x > 0.0 then "pos" else "non-pos"
        }
      }
      sign(1.0)|}

let tc_family_int_return_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[x]) -> float { x + 1 }|}

let tc_family_float_return_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(float[x]) -> int { x + 1.0 }|}

let tc_family_int_return_string_mismatch ctxt =
  assert_error ~ctxt ~code:(Some E_Type_TypeMismatch)
    {|let f(int[x]) -> string { x + 1 }|}

let tc_family_int_in_adt_field ctxt =
  assert_ok ~ctxt
    {|data Wrapper = { | Wrap(int) }
      let make(int[x]) -> Wrapper { Wrap(x) }
      make(42)|}

let tc_family_int_from_adt_match ctxt =
  assert_ok ~ctxt
    {|data Box = { | Box(int) }
      let add_box(int[n], b: Box) -> int {
        match b { | Box(v) -> n + v }
      }
      add_box(1, Box(2))|}

let tc_family_float_in_adt_field ctxt =
  assert_ok ~ctxt
    {|data Point = { | Point(float, float) }
      let make_point(float[x, y]) -> Point { Point(x, y) }
      make_point(1.0, 2.0)|}

let suite =
  "family tests"
  >::: [
         "[family] int ops" >:: tc_family_int_ops;
         "[family] float ops" >:: tc_family_float_ops;
         "[family] int rejects float" >:: tc_family_int_rejects_float;
         "[family] float rejects int" >:: tc_family_float_rejects_int;
         "[family] int comparison" >:: tc_family_int_comparison;
         "[family] float comparison" >:: tc_family_float_comparison;
         "[family] mixed int float rejected"
         >:: tc_family_mixed_int_float_rejected;
         "[family] uniform three int" >:: tc_family_uniform_three_int;
         "[family] uniform three float" >:: tc_family_uniform_three_float;
         "[family] return annotation int" >:: tc_family_return_annotation_int;
         "[family] return annotation float"
         >:: tc_family_return_annotation_float;
         "[family] mixed uniform and typed"
         >:: tc_family_mixed_uniform_and_typed;
         "[family] int i32" >:: tc_family_int_i32;
         "[family] int i64" >:: tc_family_int_i64;
         "[family] float f32" >:: tc_family_float_f32;
         "[family] float f64" >:: tc_family_float_f64;
         "[family] int subtypes no mix" >:: tc_family_int_subtypes_no_mix;
         "[family] in recursive function" >:: tc_family_in_recursive_function;
         "[family] in match" >:: tc_family_in_match;
         "[family] poly with family" >:: tc_family_poly_with_family;
         "[family] i8 basic" >:: tc_family_i8_basic;
         "[family] i16 basic" >:: tc_family_i16_basic;
         "[family] i8 arithmetic" >:: tc_family_i8_arithmetic;
         "[family] i16 arithmetic" >:: tc_family_i16_arithmetic;
         "[family] i32 arithmetic" >:: tc_family_i32_arithmetic;
         "[family] i64 arithmetic" >:: tc_family_i64_arithmetic;
         "[family] i8 comparison" >:: tc_family_i8_comparison;
         "[family] i16 comparison" >:: tc_family_i16_comparison;
         "[family] i32 comparison" >:: tc_family_i32_comparison;
         "[family] i64 comparison" >:: tc_family_i64_comparison;
         "[family] i8 rejects i16" >:: tc_family_i8_rejects_i16;
         "[family] i16 rejects i32" >:: tc_family_i16_rejects_i32;
         "[family] i32 rejects i64" >:: tc_family_i32_rejects_i64;
         "[family] i8 rejects float" >:: tc_family_i8_rejects_float;
         "[family] i64 rejects float" >:: tc_family_i64_rejects_float;
         "[family] f32 arithmetic" >:: tc_family_f32_arithmetic;
         "[family] f64 arithmetic" >:: tc_family_f64_arithmetic;
         "[family] f32 comparison" >:: tc_family_f32_comparison;
         "[family] f64 comparison" >:: tc_family_f64_comparison;
         "[family] f32 rejects f64" >:: tc_family_f32_rejects_f64;
         "[family] f64 rejects f32" >:: tc_family_f64_rejects_f32;
         "[family] f32 rejects int" >:: tc_family_f32_rejects_int;
         "[family] f64 rejects int" >:: tc_family_f64_rejects_int;
         "[family] int accepts i8" >:: tc_family_int_accepts_i8;
         "[family] int accepts i16" >:: tc_family_int_accepts_i16;
         "[family] int accepts i32" >:: tc_family_int_accepts_i32;
         "[family] int accepts i64" >:: tc_family_int_accepts_i64;
         "[family] int rejects f32" >:: tc_family_int_rejects_f32;
         "[family] int rejects f64" >:: tc_family_int_rejects_f64;
         "[family] int uniform four" >:: tc_family_int_uniform_four;
         "[family] int in list" >:: tc_family_int_in_list;
         "[family] float accepts f32" >:: tc_family_float_accepts_f32;
         "[family] float accepts f64" >:: tc_family_float_accepts_f64;
         "[family] float rejects i8" >:: tc_family_float_rejects_i8;
         "[family] float rejects i32" >:: tc_family_float_rejects_i32;
         "[family] float uniform two" >:: tc_family_float_uniform_two;
         "[family] int with string param" >:: tc_family_int_with_string_param;
         "[family] float with bool param" >:: tc_family_float_with_bool_param;
         "[family] int with list param" >:: tc_family_int_with_list_param;
         "[family] multiple groups" >:: tc_family_multiple_groups;
         "[family] int group wrong mixed call"
         >:: tc_family_int_group_wrong_mixed_call;
         "[family] int in if" >:: tc_family_int_in_if;
         "[family] float in if" >:: tc_family_float_in_if;
         "[family] int in match literal" >:: tc_family_int_in_match_literal;
         "[family] float in match wildcard"
         >:: tc_family_float_in_match_wildcard;
         "[family] int return mismatch" >:: tc_family_int_return_mismatch;
         "[family] float return mismatch" >:: tc_family_float_return_mismatch;
         "[family] int return string mismatch"
         >:: tc_family_int_return_string_mismatch;
         "[family] int in adt field" >:: tc_family_int_in_adt_field;
         "[family] int from adt match" >:: tc_family_int_from_adt_match;
         "[family] float in adt field" >:: tc_family_float_in_adt_field;
       ]
