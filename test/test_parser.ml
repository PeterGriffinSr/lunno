open OUnit2
open Lunno_frontend
open Lunno_common
open Test_helpers

let test_integer_literal ctxt =
  match parse_expr "42" with
  | Literal (LInt 42L, _) -> ()
  | _ -> assert_failure "Expected integer literal"

let test_float_literal ctxt =
  match parse_expr "3.14" with
  | Literal (LFloat f, _) when abs_float (f -. 3.14) < 0.001 -> ()
  | _ -> assert_failure "Expected float literal"

let test_string_literal ctxt =
  match parse_expr "\"hello\"" with
  | Literal (LString "hello", _) -> ()
  | _ -> assert_failure "Expected string literal"

let test_variable ctxt =
  match parse_expr "x" with
  | Variable ("x", _) -> ()
  | _ -> assert_failure "Expected variable"

let test_simple_addition ctxt =
  match parse_expr "1 + 2" with
  | Binary
      {
        binary_op = OpAdd;
        left = Literal (LInt 1L, _);
        right = Literal (LInt 2L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected addition"

let test_addition_left_associative ctxt =
  match parse_expr "1 + 2 + 3" with
  | Binary
      {
        binary_op = OpAdd;
        left =
          Binary
            {
              binary_op = OpAdd;
              left = Literal (LInt 1L, _);
              right = Literal (LInt 2L, _);
              _;
            };
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected left-associative addition"

let test_multiplication_precedence ctxt =
  match parse_expr "1 + 2 * 3" with
  | Binary
      {
        binary_op = OpAdd;
        left = Literal (LInt 1L, _);
        right =
          Binary
            {
              binary_op = OpMul;
              left = Literal (LInt 2L, _);
              right = Literal (LInt 3L, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected multiplication to bind tighter"

let test_parentheses ctxt =
  match parse_expr "(1 + 2) * 3" with
  | Binary
      {
        binary_op = OpMul;
        left = Binary { binary_op = OpAdd; _ };
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected parentheses to override precedence"

let test_subtraction ctxt =
  match parse_expr "5 - 3" with
  | Binary
      {
        binary_op = OpSub;
        left = Literal (LInt 5L, _);
        right = Literal (LInt 3L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected subtraction"

let test_division ctxt =
  match parse_expr "10 / 2" with
  | Binary
      {
        binary_op = OpDiv;
        left = Literal (LInt 10L, _);
        right = Literal (LInt 2L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected division"

let test_mixed_arithmetic ctxt =
  match parse_expr "1 + 2 - 3 * 4 / 5" with
  | Binary { binary_op = OpSub; left = Binary { binary_op = OpAdd; _ }; _ } ->
      ()
  | _ -> assert_failure "Expected mixed arithmetic with correct precedence"

let test_comparison ctxt =
  match parse_expr "x < y" with
  | Binary
      {
        binary_op = OpLess;
        left = Variable ("x", _);
        right = Variable ("y", _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected less-than comparison"

let test_comparison_precedence ctxt =
  match parse_expr "x + 1 < y * 2" with
  | Binary
      {
        binary_op = OpLess;
        left = Binary { binary_op = OpAdd; _ };
        right = Binary { binary_op = OpMul; _ };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected comparison to have lower precedence"

let test_simple_let ctxt =
  match parse_expr "let x = 42" with
  | Let { name = "x"; ty = None; let_body = Literal (LInt 42L, _); _ } -> ()
  | _ -> assert_failure "Expected let binding"

let test_typed_let ctxt =
  match parse_expr "let x: int = 42" with
  | Let
      {
        name = "x";
        ty = Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ });
        let_body = Literal (LInt 42L, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected typed let binding"

let test_single_expr_block ctxt =
  match parse_expr "{ 42 }" with
  | Block ([ Literal (LInt 42L, _) ], _) -> ()
  | _ -> assert_failure "Expected single-expression block"

let test_multi_expr_block ctxt =
  match parse_expr "{ let x = 1\n x + 2 }" with
  | Block ([ Let _; Binary _ ], _) -> ()
  | _ -> assert_failure "Expected multi-expression block"

let test_function_call_no_args ctxt =
  match parse_expr "foo()" with
  | Apply (Variable ("foo", _), [], _) -> ()
  | _ -> assert_failure "Expected function call with no arguments"

let test_function_call_single_arg ctxt =
  match parse_expr "foo(42)" with
  | Apply (Variable ("foo", _), [ Literal (LInt 42L, _) ], _) -> ()
  | _ -> assert_failure "Expected function call with single argument"

let test_function_call_multiple_args ctxt =
  match parse_expr "foo(1, 2, 3)" with
  | Apply (Variable ("foo", _), [ _; _; _ ], _) -> ()
  | _ -> assert_failure "Expected function call with multiple arguments"

let test_function_call_nested ctxt =
  match parse_expr "foo(bar(baz()))" with
  | Apply (Variable ("foo", _), [ Apply (Variable ("bar", _), _, _) ], _) -> ()
  | _ -> assert_failure "Expected nested function calls"

let test_function_call_with_arithmetic ctxt =
  match parse_expr "foo(x + 1, y * 2)" with
  | Apply
      ( Variable ("foo", _),
        [ Binary { binary_op = OpAdd; _ }; Binary { binary_op = OpMul; _ } ],
        _ ) ->
      ()
  | _ -> assert_failure "Expected function call with arithmetic arguments"

let test_chained_function_calls ctxt =
  match parse_expr "foo()()()" with
  | Apply (Apply (Apply (Variable ("foo", _), [], _), [], _), [], _) -> ()
  | _ -> assert_failure "Expected chained function calls"

let test_function_no_params ctxt = assert_parses ~ctxt "let f() { 42 }"

let test_function_untyped_params ctxt =
  match parse_expr "let f(x, y) { x + y }" with
  | Let { name = "f"; let_body = Lambda { params; ret_ty = None; _ }; _ } -> (
      assert_equal ~ctxt 2 (List.length params);
      match params with
      | [ p1; p2 ] ->
          assert_equal ~ctxt "x" p1.param_name;
          assert_equal ~ctxt "y" p2.param_name;
          assert_equal ~ctxt None p1.param_ty;
          assert_equal ~ctxt None p2.param_ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function definition"

let test_function_typed_params ctxt =
  match parse_expr "let f(x: int, y: int) -> int { x + y }" with
  | Let
      {
        name = "f";
        let_body =
          Lambda
            {
              params;
              ret_ty = Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ });
              _;
            };
        _;
      } -> (
      match params with
      | [ p1; p2 ] -> (
          (match p1.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p1");
          match p2.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p2")
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected typed function"

let test_function_mixed_params ctxt =
  match parse_expr "let f(x: int, y) -> int { x + y }" with
  | Let { let_body = Lambda { params; _ }; _ } -> (
      match params with
      | [ p1; p2 ] ->
          (match p1.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p1");
          assert_equal ~ctxt None p2.param_ty
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function with mixed params"

let test_function_uniform_syntax ctxt =
  match parse_expr "let f(int[x, y]) -> int { x + y }" with
  | Let { let_body = Lambda { params; _ }; _ } -> (
      match params with
      | [ p1; p2 ] -> (
          assert_equal ~ctxt "x" p1.param_name;
          assert_equal ~ctxt "y" p2.param_name;
          (match p1.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p1");
          match p2.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p2")
      | _ -> assert_failure "Expected 2 params")
  | _ -> assert_failure "Expected function with uniform syntax"

let test_function_mixed_uniform_syntax ctxt =
  match parse_expr "let f(int[x, y], z: string) -> int { x + y }" with
  | Let { let_body = Lambda { params; _ }; _ } -> (
      assert_equal ~ctxt 3 (List.length params);
      match params with
      | [ p1; p2; p3 ] ->
          (match p1.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p1");
          (match p2.param_ty with
          | Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) -> ()
          | _ -> assert_failure "Expected FInt family meta for p2");
          assert_equal ~ctxt (Some Ast.TyString) p3.param_ty
      | _ -> assert_failure "Expected 3 params")
  | _ -> assert_failure "Expected function with mixed uniform syntax"

let test_multiple_top_level_exprs ctxt =
  let prog = parse_program "let x = 1\nlet y = 2\nx + y" in
  assert_equal ~ctxt 3 (List.length prog.body)

let test_nested_functions ctxt =
  assert_parses ~ctxt
    "let outer(x: int) { let inner(y: int) { x + y } inner(10) }"

let test_function_call_in_body ctxt =
  assert_parses ~ctxt "let f(x: int) -> int { let double = x * x\n x + double }"

let test_empty_block_fails ctxt =
  try
    let _ = parse_expr "{}" in
    assert_failure "Empty block should fail"
  with _ -> ()

let test_deeply_nested_arithmetic ctxt =
  assert_parses ~ctxt "1 + 2 * 3 + 4 * 5 + 6"

let test_all_comparison_ops ctxt =
  assert_parses ~ctxt "x < y";
  assert_parses ~ctxt "x > y";
  assert_parses ~ctxt "x = y";
  assert_parses ~ctxt "x <> y"

let test_block_three_exprs ctxt =
  match parse_expr "{ let x = 1\n let y = 2\n x + y }" with
  | Block ([ Let _; Let _; Binary _ ], _) -> ()
  | _ -> assert_failure "Expected block with three expressions"

let test_nested_blocks ctxt =
  match parse_expr "{ { 1 } }" with
  | Block ([ Block ([ Literal (LInt 1L, _) ], _) ], _) -> ()
  | _ -> assert_failure "Expected nested blocks"

let test_block_with_function_call ctxt =
  match parse_expr "{ foo()\n bar() }" with
  | Block ([ Apply _; Apply _ ], _) -> ()
  | _ -> assert_failure "Expected block with function calls"

let test_let_with_arithmetic_body ctxt =
  match parse_expr "let x = 1 + 2 * 3" with
  | Let { let_body = Binary { binary_op = OpAdd; _ }; _ } -> ()
  | _ -> assert_failure "Expected let with arithmetic body"

let test_let_with_function_call_body ctxt =
  match parse_expr "let x = foo(42)" with
  | Let { let_body = Apply _; _ } -> ()
  | _ -> assert_failure "Expected let with function call body"

let test_let_with_block_body ctxt =
  match parse_expr "let x = { let y = 1\n y + 1 }" with
  | Let { let_body = Block _; _ } -> ()
  | _ -> assert_failure "Expected let with block body"

let test_sequential_lets ctxt =
  let prog = parse_program "let x = 1\nlet y = 2\nlet z = 3" in
  match prog.body with
  | [ Let _; Let _; Let _ ] -> ()
  | _ -> assert_failure "Expected three sequential lets"

let test_recursive_function_detection ctxt =
  match parse_expr "let fib(n: int) -> int { fib(n - 1) }" with
  | Let { let_body = Lambda { is_recursive = true; _ }; _ } -> ()
  | _ -> assert_failure "Expected recursive function to be detected"

let test_non_recursive_function ctxt =
  match parse_expr "let id(x: int) -> int { x }" with
  | Let { let_body = Lambda { is_recursive = false; _ }; _ } -> ()
  | _ -> assert_failure "Expected non-recursive function"

let test_function_with_multiple_return_points ctxt =
  assert_parses ~ctxt "let f(x: int) -> int { if x > 0 then x else 0 }"

let test_function_returning_function ctxt =
  assert_parses ~ctxt
    "let outer(x: int) -> int { let inner(y: int) -> int { x + y }\n inner }"

let test_simple_if_else ctxt =
  match parse_expr "if true then 1 else 0" with
  | If
      {
        cond = Literal (LBool true, _);
        then_ = Literal (LInt 1L, _);
        else_ = Some (Literal (LInt 0L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected simple if-else"

let test_if_with_comparison ctxt =
  match parse_expr "if x > 0 then 1 else 1" with
  | If
      {
        cond = Binary { binary_op = OpGreater; _ };
        then_ = Literal (LInt 1L, _);
        else_ = Some (Literal (LInt 1L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if with comparison"

let test_if_without_else ctxt =
  match parse_expr "if x > 0 then x" with
  | If
      {
        cond = Binary { binary_op = OpGreater; _ };
        then_ = Variable ("x", _);
        else_ = None;
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if without else"

let test_nested_if_in_then ctxt =
  match parse_expr "if x > 0 then if y > 0 then 1 else 2 else 3" with
  | If
      {
        cond = Binary { binary_op = OpGreater; left = Variable ("x", _); _ };
        then_ =
          If
            {
              cond =
                Binary { binary_op = OpGreater; left = Variable ("y", _); _ };
              then_ = Literal (LInt 1L, _);
              else_ = Some (Literal (LInt 2L, _));
              _;
            };
        else_ = Some (Literal (LInt 3L, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested if in then branch"

let test_nested_if_in_else ctxt =
  match parse_expr "if x > 0 then 1 else if y > 0 then 2 else 3" with
  | If
      {
        then_ = Literal (LInt 1L, _);
        else_ =
          Some
            (If
               {
                 then_ = Literal (LInt 2L, _);
                 else_ = Some (Literal (LInt 3L, _));
                 _;
               });
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested if in else branch"

let test_dangling_else_associates_nearest ctxt =
  match parse_expr "if x > 0 then if y > 0 then 1 else 2" with
  | If
      {
        cond = Binary { binary_op = OpGreater; left = Variable ("x", _); _ };
        then_ =
          If
            {
              cond =
                Binary { binary_op = OpGreater; left = Variable ("y", _); _ };
              then_ = Literal (LInt 1L, _);
              else_ = Some (Literal (LInt 2L, _));
              _;
            };
        else_ = None;
        _;
      } ->
      ()
  | _ -> assert_failure "Expected else to bind to nearest if"

let test_if_with_block_branches ctxt =
  match parse_expr "if x > 0 then { let y = x\n y } else { 0 }" with
  | If
      {
        then_ = Block ([ Let _; Variable _ ], _);
        else_ = Some (Block ([ Literal (LInt 0L, _) ], _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected if with block branches"

let test_if_with_block_condition ctxt =
  assert_parses ~ctxt "if { let x = compute()\n x > 0 } then 1 else 0"

let test_if_then_arithmetic ctxt =
  match parse_expr "if x > 0 then x + 1 else x - 1" with
  | If
      {
        then_ = Binary { binary_op = OpAdd; _ };
        else_ = Some (Binary { binary_op = OpSub; _ });
        _;
      } ->
      ()
  | _ -> assert_failure "Expected arithmetic in both branches"

let test_if_then_function_call ctxt =
  match parse_expr "if ready then start() else wait()" with
  | If
      {
        then_ = Apply (Variable ("start", _), [], _);
        else_ = Some (Apply (Variable ("wait", _), [], _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected function calls in branches"

let test_if_with_complex_condition ctxt =
  assert_parses ~ctxt "if x > 0 + y * 2 then 1 else 0"

let test_if_with_multiple_comparisons ctxt =
  assert_parses ~ctxt "if x > 0 then if y < 10 then 1 else 2 else 3"

let test_if_with_equality ctxt =
  match parse_expr "if x = 0 then 1 else 0" with
  | If { cond = Binary { binary_op = OpEqual; _ }; _ } -> ()
  | _ -> assert_failure "Expected equality comparison"

let test_if_with_inequality ctxt =
  match parse_expr "if x <> 0 then 1 else 0" with
  | If { cond = Binary { binary_op = OpNotEqual; _ }; _ } -> ()
  | _ -> assert_failure "Expected inequality comparison"

let test_sequential_ifs ctxt =
  let prog = parse_program "if x > 0 then 1 else 0\nif y > 0 then 2 else 0" in
  match prog.body with
  | [ If _; If _ ] -> ()
  | _ -> assert_failure "Expected two separate if expressions"

let test_if_after_let ctxt =
  let prog = parse_program "let x = 42\nif x > 0 then x else 0" in
  match prog.body with
  | [ Let _; If _ ] -> ()
  | _ -> assert_failure "Expected let followed by if"

let test_if_with_parenthesized_condition ctxt =
  assert_parses ~ctxt "if (x > 0) then 1 else 0"

let test_if_with_parenthesized_branches ctxt =
  assert_parses ~ctxt "if x > 0 then (1) else (0)"

let test_deeply_nested_ifs ctxt =
  assert_parses ~ctxt "if a then if b then if c then 1 else 2 else 3 else 4"

let test_if_chain_else_if_pattern ctxt =
  assert_parses ~ctxt "if x < 0 then 1 else if x > 0 then 1 else 0"

let test_if_with_let_in_branches ctxt =
  assert_parses ~ctxt "if x > 0 then let y = x else let z = 0"

let test_if_without_else_in_block ctxt =
  match parse_expr "{ if x > 0 then x }" with
  | Block ([ If { else_ = None; _ } ], _) -> ()
  | _ -> assert_failure "Expected if without else inside block"

let test_if_with_string_branches ctxt =
  match parse_expr "if x > 0 then \"positive\" else \"non-positive\"" with
  | If
      {
        then_ = Literal (LString "positive", _);
        else_ = Some (Literal (LString "non-positive", _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected string literals in branches"

let test_if_with_boolean_literals ctxt =
  match parse_expr "if x then true else false" with
  | If
      {
        cond = Variable ("x", _);
        then_ = Literal (LBool true, _);
        else_ = Some (Literal (LBool false, _));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected boolean literals in branches"

let test_if_result_in_let ctxt =
  match parse_expr "let result = if x > 0 then 1 else 0" with
  | Let { let_body = If _; _ } -> ()
  | _ -> assert_failure "Expected if expression as let body"

let test_match_simple_literal ctxt =
  match parse_expr "match x { | 0 -> 1 | 1 -> 2 }" with
  | Match
      {
        scrutinee = Variable ("x", _);
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              guard = None;
              case_body = Literal (LInt 1L, _);
              _;
            };
            {
              pattern = PIntLiteral (1L, _);
              guard = None;
              case_body = Literal (LInt 2L, _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected simple match with literal patterns"

let test_match_wildcard ctxt =
  match parse_expr "match x { | _ -> 0 }" with
  | Match
      {
        cases =
          [ { pattern = PWildcard _; case_body = Literal (LInt 0L, _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with wildcard pattern"

let test_match_variable_pattern ctxt =
  match parse_expr "match x { | y -> y }" with
  | Match
      {
        cases =
          [ { pattern = PVariable ("y", _); case_body = Variable ("y", _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with variable pattern"

let test_match_nil_pattern ctxt =
  match parse_expr "match lst { | [] -> 0 }" with
  | Match
      {
        cases = [ { pattern = PNil _; case_body = Literal (LInt 0L, _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with nil pattern"

let test_match_cons_pattern ctxt =
  match parse_expr "match lst { | x :: xs -> x }" with
  | Match
      {
        cases =
          [
            {
              pattern = PCons (PVariable ("x", _), PVariable ("xs", _), _);
              case_body = Variable ("x", _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with cons pattern"

let test_match_nested_cons_pattern ctxt =
  match parse_expr "match lst { | x :: y :: zs -> x }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PCons
                  ( PVariable ("x", _),
                    PCons (PVariable ("y", _), PVariable ("zs", _), _),
                    _ );
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with nested cons pattern"

let test_match_with_guard ctxt =
  match parse_expr "match x { | n if n > 0 -> n }" with
  | Match
      {
        cases =
          [
            {
              pattern = PVariable ("n", _);
              guard = Some (Binary { binary_op = OpGreater; _ });
              case_body = Variable ("n", _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with guard"

let test_match_multiple_guards ctxt =
  match
    parse_expr "match x { | n if n > 0 -> 1 | m if m < 0 -> 1 | _ -> 0 }"
  with
  | Match
      {
        cases =
          [
            { guard = Some (Binary { binary_op = OpGreater; _ }); _ };
            { guard = Some (Binary { binary_op = OpLess; _ }); _ };
            { pattern = PWildcard _; guard = None; _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with multiple guards"

let test_match_string_literal ctxt =
  match parse_expr "match s { | \"hello\" -> 1 | \"world\" -> 2 }" with
  | Match
      {
        cases =
          [
            { pattern = PStringLiteral ("hello", _); _ };
            { pattern = PStringLiteral ("world", _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with string literal patterns"

let test_match_bool_literal ctxt =
  match parse_expr "match b { | true -> 1 | false -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PBooleanLiteral (true, _); _ };
            { pattern = PBooleanLiteral (false, _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with boolean literal patterns"

let test_match_float_literal ctxt =
  match parse_expr "match f { | 3.14 -> 1 | 2.71 -> 2 }" with
  | Match
      {
        cases =
          [
            { pattern = PFloatLiteral (f1, _); _ };
            { pattern = PFloatLiteral (f2, _); _ };
          ];
        _;
      } ->
      assert_bool "Expected 3.14" (abs_float (f1 -. 3.14) < 0.001);
      assert_bool "Expected 2.71" (abs_float (f2 -. 2.71) < 0.001)
  | _ -> assert_failure "Expected match with float literal patterns"

let test_match_complex_body ctxt =
  match parse_expr "match x {| 0 -> { let y = 10\ny + 5 } }" with
  | Match
      {
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              case_body = Block ([ Let _; Binary { binary_op = OpAdd; _ } ], _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex body"

let test_match_block_body ctxt =
  match parse_expr "match x { | 0 -> { let y = 10\n y } }" with
  | Match
      {
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              case_body = Block ([ Let _; _ ], _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with block body"

let test_match_nested_match ctxt =
  match parse_expr "match x { | 0 -> match y { | 1 -> 2 } }" with
  | Match
      {
        cases = [ { pattern = PIntLiteral (0L, _); case_body = Match _; _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested match"

let test_match_in_function ctxt =
  match parse_expr "let f(x: int) -> int { match x { | 0 -> 1 | _ -> x } }" with
  | Let
      {
        let_body =
          Lambda
            {
              lambda_body =
                Block
                  ( [
                      Match
                        {
                          cases =
                            [
                              { pattern = PIntLiteral (0L, _); _ };
                              { pattern = PWildcard _; _ };
                            ];
                          _;
                        };
                    ],
                    _ );
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match in function body"

let test_match_complex_scrutinee ctxt =
  match parse_expr "match x + 1 { | 2 -> 1 }" with
  | Match
      {
        scrutinee = Binary { binary_op = OpAdd; _ };
        cases = [ { pattern = PIntLiteral (2L, _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex scrutinee"

let test_match_function_call_scrutinee ctxt =
  match parse_expr "match f(x) { | 0 -> 1 }" with
  | Match { scrutinee = Apply (Variable ("f", _), [ Variable ("x", _) ], _); _ }
    ->
      ()
  | _ -> assert_failure "Expected match with function call scrutinee"

let test_match_parenthesized_pattern ctxt =
  match parse_expr "match x { | (y) -> y }" with
  | Match
      {
        cases =
          [ { pattern = PVariable ("y", _); case_body = Variable ("y", _); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with parenthesized pattern"

let test_match_cons_with_nil ctxt =
  match parse_expr "match lst { | x :: [] -> x }" with
  | Match
      { cases = [ { pattern = PCons (PVariable ("x", _), PNil _, _); _ } ]; _ }
    ->
      ()
  | _ -> assert_failure "Expected match with cons to nil"

let test_match_multiple_cases ctxt =
  match
    parse_expr "match x { | 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 4 | _ -> 0 }"
  with
  | Match { cases; _ } -> assert_equal ~ctxt 5 (List.length cases)
  | _ -> assert_failure "Expected match with 5 cases"

let test_match_guard_with_equality ctxt =
  match parse_expr "match x { | n if n = 0 -> 1 }" with
  | Match
      { cases = [ { guard = Some (Binary { binary_op = OpEqual; _ }); _ } ]; _ }
    ->
      ()
  | _ -> assert_failure "Expected match with equality guard"

let test_match_guard_with_complex_expr ctxt =
  match parse_expr "match x { | n if n > 0 + 1 -> n }" with
  | Match
      {
        cases =
          [
            {
              guard =
                Some
                  (Binary
                     {
                       binary_op = OpGreater;
                       right = Binary { binary_op = OpAdd; _ };
                       _;
                     });
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with complex guard expression"

let test_match_result_in_let ctxt =
  match parse_expr "let result = match x { | 0 -> 1 | _ -> 0 }" with
  | Let { let_body = Match _; _ } -> ()
  | _ -> assert_failure "Expected match as let body"

let test_match_as_function_argument ctxt =
  match parse_expr "f(match x { | 0 -> 1 | _ -> 0 })" with
  | Apply (Variable ("f", _), [ Match _ ], _) -> ()
  | _ -> assert_failure "Expected match as function argument"

let test_match_sequential ctxt =
  let prog = parse_program "match x { | 0 -> 1 }\nmatch y { | 0 -> 2 }" in
  match prog.body with
  | [ Match _; Match _ ] -> ()
  | _ -> assert_failure "Expected two sequential matches"

let test_match_after_let ctxt =
  let prog = parse_program "let x = 42\nmatch x { | 42 -> 1 }" in
  match prog.body with
  | [ Let _; Match _ ] -> ()
  | _ -> assert_failure "Expected let followed by match"

let test_match_cons_right_associative ctxt =
  match parse_expr "match lst { | a :: b :: c :: [] -> a }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PCons
                  ( PVariable ("a", _),
                    PCons
                      ( PVariable ("b", _),
                        PCons (PVariable ("c", _), PNil _, _),
                        _ ),
                    _ );
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected right-associative cons pattern"

let test_match_guard_with_function_call ctxt =
  match parse_expr "match x { | n if is_positive(n) -> n }" with
  | Match
      {
        cases =
          [ { guard = Some (Apply (Variable ("is_positive", _), _, _)); _ } ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected match with function call in guard"

let test_match_literal_then_wildcard ctxt =
  match parse_expr "match x { | 0 -> 1 | 1 -> 2 | _ -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PIntLiteral (0L, _); _ };
            { pattern = PIntLiteral (1L, _); _ };
            { pattern = PWildcard _; _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected literals followed by wildcard"

let test_match_fibonacci_style ctxt =
  match
    parse_expr "match n { | 0 -> 0 | 1 -> 1 | m -> fib(m - 1) + fib(m - 2) }"
  with
  | Match
      {
        cases =
          [
            {
              pattern = PIntLiteral (0L, _);
              case_body = Literal (LInt 0L, _);
              _;
            };
            {
              pattern = PIntLiteral (1L, _);
              case_body = Literal (LInt 1L, _);
              _;
            };
            {
              pattern = PVariable ("m", _);
              case_body = Binary { binary_op = OpAdd; _ };
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected fibonacci-style match"

let test_match_list_length_style ctxt =
  match parse_expr "match lst { | [] -> 0 | _ :: xs -> 1 + length(xs) }" with
  | Match
      {
        cases =
          [
            { pattern = PNil _; case_body = Literal (LInt 0L, _); _ };
            {
              pattern = PCons (PWildcard _, PVariable ("xs", _), _);
              case_body = Binary { binary_op = OpAdd; _ };
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected list length style match"

let test_match_with_if_in_body ctxt =
  match parse_expr "match x { | n -> if n > 0 then n else 0 }" with
  | Match
      { cases = [ { pattern = PVariable ("n", _); case_body = If _; _ } ]; _ }
    ->
      ()
  | _ -> assert_failure "Expected match with if in body"

let test_match_exhaustive_bool ctxt =
  match parse_expr "match b { | true -> 1 | false -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PBooleanLiteral (true, _); _ };
            { pattern = PBooleanLiteral (false, _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected exhaustive boolean match"

let test_adt_simple_declaration ctxt =
  let prog = parse_program "data Color = { | Red | Green | Blue }" in
  match prog.type_decls with
  | [ { type_name = "Color"; variants; _ } ] ->
      assert_equal ~ctxt 3 (List.length variants);
      assert_equal ~ctxt "Red" (List.nth variants 0).variant_name;
      assert_equal ~ctxt "Green" (List.nth variants 1).variant_name;
      assert_equal ~ctxt "Blue" (List.nth variants 2).variant_name
  | _ -> assert_failure "Expected single ADT declaration"

let test_adt_single_variant ctxt =
  let prog = parse_program "data Unit = { | Only }" in
  match prog.type_decls with
  | [ { type_name = "Unit"; variants = [ { variant_name = "Only"; _ } ]; _ } ]
    ->
      ()
  | _ -> assert_failure "Expected single-variant ADT"

let test_adt_variant_with_fields ctxt =
  let prog = parse_program "data Option = { | Some(int) | None }" in
  match prog.type_decls with
  | [ { variants; _ } ] -> (
      match variants with
      | [ some; none ] ->
          assert_equal ~ctxt "Some" some.variant_name;
          assert_equal ~ctxt 1 (List.length some.variant_fields);
          assert_equal ~ctxt "None" none.variant_name;
          assert_equal ~ctxt 0 (List.length none.variant_fields)
      | _ -> assert_failure "Expected 2 variants")
  | _ -> assert_failure "Expected Option ADT"

let test_adt_result_type ctxt =
  let prog = parse_program "data Result = { | Ok(int) | Err(string) }" in
  match prog.type_decls with
  | [ { type_name = "Result"; variants; _ } ] -> (
      assert_equal ~ctxt 2 (List.length variants);
      (match (List.nth variants 0).variant_fields with
      | [ Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ] -> ()
      | _ -> assert_failure "Expected FInt family meta for Ok field");
      match (List.nth variants 1).variant_fields with
      | [ Ast.TyString ] -> ()
      | _ -> assert_failure "Expected TyString for Err field")
  | _ -> assert_failure "Expected Result ADT"

let test_adt_multi_field_variant ctxt =
  let prog = parse_program "data Pair = { | Pair(int, int) }" in
  match prog.type_decls with
  | [ { variants = [ { variant_name = "Pair"; variant_fields; _ } ]; _ } ] ->
      assert_equal ~ctxt 2 (List.length variant_fields)
  | _ -> assert_failure "Expected Pair ADT with two fields"

let test_adt_multiple_declarations ctxt =
  let prog =
    parse_program
      "data Color = { | Red | Green }\ndata Shape = { | Circle | Square }"
  in
  assert_equal ~ctxt 2 (List.length prog.type_decls)

let test_adt_constructor_expr ctxt =
  match parse_expr "Some(42)" with
  | Apply (Variable ("Some", _), [ Literal (LInt 42L, _) ], _) -> ()
  | _ -> assert_failure "Expected constructor application"

let test_adt_constructor_no_args ctxt =
  match parse_expr "None" with
  | Variable ("None", _) | Constructor ("None", [], _) -> ()
  | _ -> assert_failure "Expected nullary constructor"

let test_adt_constructor_multiple_args ctxt =
  match parse_expr "Pair(1, 2)" with
  | Apply
      (Variable ("Pair", _), [ Literal (LInt 1L, _); Literal (LInt 2L, _) ], _)
    ->
      ()
  | _ -> assert_failure "Expected constructor with two args"

let test_adt_constructor_nested ctxt =
  match parse_expr "Some(Some(42))" with
  | Apply
      ( Variable ("Some", _),
        [ Apply (Variable ("Some", _), [ Literal (LInt 42L, _) ], _) ],
        _ ) ->
      ()
  | _ -> assert_failure "Expected nested constructor"

let test_adt_constructor_in_let ctxt =
  match parse_expr "let x = Some(42)" with
  | Let
      {
        let_body = Apply (Variable ("Some", _), [ Literal (LInt 42L, _) ], _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected constructor as let body"

let test_adt_constructor_pattern ctxt =
  match parse_expr "match x { | Some(n) -> n | None -> 0 }" with
  | Match
      {
        cases =
          [
            {
              pattern = PConstructor ("Some", [ PVariable ("n", _) ], _);
              case_body = Variable ("n", _);
              _;
            };
            {
              pattern = PVariable ("None", _);
              case_body = Literal (LInt 0L, _);
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected constructor patterns in match"

let test_adt_constructor_pattern_multi_field ctxt =
  match parse_expr "match p { | Pair(a, b) -> a + b }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PConstructor
                  ("Pair", [ PVariable ("a", _); PVariable ("b", _) ], _);
              case_body = Binary { binary_op = OpAdd; _ };
              _;
            };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected multi-field constructor pattern"

let test_adt_constructor_pattern_nested ctxt =
  match parse_expr "match x { | Some(Some(n)) -> n | _ -> 0 }" with
  | Match
      {
        cases =
          [
            {
              pattern =
                PConstructor
                  ( "Some",
                    [ PConstructor ("Some", [ PVariable ("n", _) ], _) ],
                    _ );
              _;
            };
            _;
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected nested constructor pattern"

let test_adt_constructor_pattern_with_wildcard ctxt =
  match parse_expr "match x { | Some(_) -> 1 | None -> 0 }" with
  | Match
      {
        cases =
          [
            { pattern = PConstructor ("Some", [ PWildcard _ ], _); _ };
            { pattern = PVariable ("None", _); _ };
          ];
        _;
      } ->
      ()
  | _ -> assert_failure "Expected constructor pattern with wildcard"

let test_list_literal_empty ctxt =
  match parse_expr "[]" with
  | Literal (LNil, _) -> ()
  | _ -> assert_failure "Expected empty list (LNil)"

let test_list_literal_singleton ctxt =
  match parse_expr "[1]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LInt 1L, _);
        right = Literal (LNil, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected singleton list desugared to cons"

let test_list_literal_multi ctxt =
  match parse_expr "[1, 2, 3]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LInt 1L, _);
        right =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LInt 2L, _);
              right =
                Binary
                  {
                    binary_op = OpCons;
                    left = Literal (LInt 3L, _);
                    right = Literal (LNil, _);
                    _;
                  };
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected multi-element list desugared to cons chain"

let test_list_literal_floats ctxt =
  match parse_expr "[1.0, 2.0]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LFloat _, _);
        right =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LFloat _, _);
              right = Literal (LNil, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected float list desugared to cons"

let test_list_literal_strings ctxt =
  match parse_expr "[\"a\", \"b\"]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LString "a", _);
        right =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LString "b", _);
              right = Literal (LNil, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected string list desugared to cons"

let test_list_literal_bools ctxt =
  match parse_expr "[true, false]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LBool true, _);
        right =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LBool false, _);
              right = Literal (LNil, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected bool list desugared to cons"

let test_list_literal_exprs ctxt =
  match parse_expr "[1 + 2, 3 * 4]" with
  | Binary
      {
        binary_op = OpCons;
        left = Binary { binary_op = OpAdd; _ };
        right =
          Binary
            {
              binary_op = OpCons;
              left = Binary { binary_op = OpMul; _ };
              right = Literal (LNil, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected list of expressions desugared to cons"

let test_list_literal_in_let ctxt =
  match parse_expr "let xs = [1, 2, 3]" with
  | Let { let_body = Binary { binary_op = OpCons; _ }; _ } -> ()
  | _ -> assert_failure "Expected list literal as let body"

let test_list_literal_as_arg ctxt =
  match parse_expr "f([1, 2, 3])" with
  | Apply (Variable ("f", _), [ Binary { binary_op = OpCons; _ } ], _) -> ()
  | _ -> assert_failure "Expected list literal as function argument"

let test_cons_simple ctxt =
  match parse_expr "1 :: []" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LInt 1L, _);
        right = Literal (LNil, _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected cons onto empty list"

let test_cons_right_associative ctxt =
  match parse_expr "1 :: 2 :: []" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LInt 1L, _);
        right =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LInt 2L, _);
              right = Literal (LNil, _);
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected right-associative cons"

let test_cons_onto_literal ctxt =
  match parse_expr "0 :: [1, 2, 3]" with
  | Binary
      {
        binary_op = OpCons;
        left = Literal (LInt 0L, _);
        right = Binary { binary_op = OpCons; _ };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected cons onto list literal"

let test_cons_variable ctxt =
  match parse_expr "x :: xs" with
  | Binary
      {
        binary_op = OpCons;
        left = Variable ("x", _);
        right = Variable ("xs", _);
        _;
      } ->
      ()
  | _ -> assert_failure "Expected cons of variables"

let test_cons_in_let ctxt =
  match parse_expr "let xs = 1 :: 2 :: []" with
  | Let
      {
        let_body =
          Binary
            {
              binary_op = OpCons;
              left = Literal (LInt 1L, _);
              right = Binary { binary_op = OpCons; _ };
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected cons chain as let body"

let test_type_annotation_list_int ctxt =
  match parse_expr "let xs: int list = []" with
  | Let
      {
        ty = Some (Ast.TyList (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected int list annotation"

let test_type_annotation_list_float ctxt =
  match parse_expr "let xs: float list = []" with
  | Let
      {
        ty = Some (Ast.TyList (Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ }));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected float list annotation"

let test_type_annotation_list_string ctxt =
  match parse_expr "let xs: string list = []" with
  | Let { ty = Some (Ast.TyList Ast.TyString); _ } -> ()
  | _ -> assert_failure "Expected string list annotation"

let test_type_annotation_list_bool ctxt =
  match parse_expr "let xs: bool list = []" with
  | Let { ty = Some (Ast.TyList Ast.TyBool); _ } -> ()
  | _ -> assert_failure "Expected bool list annotation"

let test_type_annotation_function_int_to_int ctxt =
  match parse_expr "let f: (int) -> int = g" with
  | Let
      {
        ty =
          Some
            (Ast.TyFunction
               ( [ Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ],
                 Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected (int) -> int annotation"

let test_type_annotation_function_multi_param ctxt =
  match parse_expr "let f: (int, int) -> int = g" with
  | Let
      {
        ty =
          Some
            (Ast.TyFunction
               ( [
                   Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ };
                   Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ };
                 ],
                 Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected (int, int) -> int annotation"

let test_type_annotation_function_no_params ctxt =
  match parse_expr "let f -> int { g }" with
  | Let
      {
        ty = None;
        let_body =
          Lambda
            {
              params = [];
              ret_ty = Some (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ });
              _;
            };
        _;
      } ->
      ()
  | _ -> assert_failure "Expected zero-param function with int return type"

let test_type_annotation_function_returns_string ctxt =
  match parse_expr "let f: (int) -> string = g" with
  | Let
      {
        ty =
          Some
            (Ast.TyFunction
               ([ Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ], Ast.TyString));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected (int) -> string annotation"

let test_type_annotation_function_returns_bool ctxt =
  match parse_expr "let f: (int) -> bool = g" with
  | Let
      {
        ty =
          Some
            (Ast.TyFunction
               ([ Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ], Ast.TyBool));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected (int) -> bool annotation"

let test_type_annotation_function_takes_list ctxt =
  match parse_expr "let f: (int list) -> int = g" with
  | Let
      {
        ty =
          Some
            (Ast.TyFunction
               ( [ Ast.TyList (Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }) ],
                 Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ));
        _;
      } ->
      ()
  | _ -> assert_failure "Expected (int list) -> int annotation"

let test_type_annotation_in_param ctxt =
  assert_parses ~ctxt "let f(xs: int list) -> int { 0 }"

let test_type_annotation_list_in_return ctxt =
  assert_parses ~ctxt "let f(x: int) -> int list { [] }"

let test_type_annotation_function_in_param ctxt =
  assert_parses ~ctxt "let apply(f: (int) -> int, x: int) -> int { f(x) }"

let test_type_annotation_function_returns_function ctxt =
  assert_parses ~ctxt "let add_n(n: int) -> (int) -> int { g }"

let test_parse_error_missing_then ctxt =
  assert_parse_fails ~ctxt "if x > 0 1 else 0"

let test_parse_error_missing_body ctxt =
  assert_parse_fails ~ctxt "let f(x: int) ->"

let test_parse_error_unclosed_paren ctxt = assert_parse_fails ~ctxt "(1 + 2"

let test_parse_error_unclosed_brace ctxt =
  assert_parse_fails ~ctxt "{ let x = 1"

let test_parse_error_unclosed_bracket ctxt = assert_parse_fails ~ctxt "[1, 2, 3"

let test_parse_error_trailing_comma_in_list ctxt =
  assert_parse_fails ~ctxt "[1, 2,]"

let test_parse_error_trailing_comma_in_args ctxt =
  assert_parse_fails ~ctxt "f(1, 2,)"

let test_parse_error_missing_arrow_in_match ctxt =
  assert_parse_fails ~ctxt "match x { | 0 1 }"

let test_parse_error_empty_match ctxt = assert_parse_fails ~ctxt "match x { }"

let test_parse_error_let_missing_equals ctxt =
  assert_parse_fails ~ctxt "let x 42"

let test_parse_error_let_missing_body ctxt = assert_parse_fails ~ctxt "let x ="

let test_parse_error_double_arrow ctxt =
  assert_parse_fails ~ctxt "let f(x: int) -> -> int { x }"

let test_parse_error_bare_arrow ctxt = assert_parse_fails ~ctxt "-> int"

let test_parse_error_match_no_pipe ctxt =
  assert_parse_fails ~ctxt "match x { 0 -> 1 }"

let test_parse_error_unclosed_match ctxt =
  assert_parse_fails ~ctxt "match x { | 0 -> 1"

let test_parse_error_cons_missing_rhs ctxt = assert_parse_fails ~ctxt "1 ::"
let test_parse_error_operator_missing_lhs ctxt = assert_parse_fails ~ctxt "+ 1"
let test_parse_error_operator_missing_rhs ctxt = assert_parse_fails ~ctxt "1 +"

let test_parse_error_double_colon_in_type ctxt =
  assert_parse_fails ~ctxt "let x: int :: int = 1"

let test_list_literal_nested ctxt = assert_parses ~ctxt "[[1, 2], [3, 4]]"

let suite =
  "parser tests"
  >::: [
         "[parser] integer literal" >:: test_integer_literal;
         "[parser] float literal" >:: test_float_literal;
         "[parser] string literal" >:: test_string_literal;
         "[parser] variable" >:: test_variable;
         "[parser] simple addition" >:: test_simple_addition;
         "[parser] addition left associative" >:: test_addition_left_associative;
         "[parser] multiplication precedence" >:: test_multiplication_precedence;
         "[parser] parentheses" >:: test_parentheses;
         "[parser] deeply nested arithmetic" >:: test_deeply_nested_arithmetic;
         "[parser] comparison" >:: test_comparison;
         "[parser] comparison precedence" >:: test_comparison_precedence;
         "[parser] all comparison ops" >:: test_all_comparison_ops;
         "[parser] simple let" >:: test_simple_let;
         "[parser] typed let" >:: test_typed_let;
         "[parser] single expr block" >:: test_single_expr_block;
         "[parser] multi expr block" >:: test_multi_expr_block;
         "[parser] empty block fails" >:: test_empty_block_fails;
         "[parser] function no params" >:: test_function_no_params;
         "[parser] function untyped params" >:: test_function_untyped_params;
         "[parser] function typed params" >:: test_function_typed_params;
         "[parser] function mixed params" >:: test_function_mixed_params;
         "[parser] function uniform syntax" >:: test_function_uniform_syntax;
         "[parser] function mixed uniform syntax"
         >:: test_function_mixed_uniform_syntax;
         "[parser] nested functions" >:: test_nested_functions;
         "[parser] function call in body" >:: test_function_call_in_body;
         "[parser] multiple top level exprs" >:: test_multiple_top_level_exprs;
         "[parser] simple if else" >:: test_simple_if_else;
         "[parser] if with comparison" >:: test_if_with_comparison;
         "[parser] if without else" >:: test_if_without_else;
         "[parser] nested if in then" >:: test_nested_if_in_then;
         "[parser] nested if in else" >:: test_nested_if_in_else;
         "[parser] dangling else associates nearest"
         >:: test_dangling_else_associates_nearest;
         "[parser] if with block branches" >:: test_if_with_block_branches;
         "[parser] if with block condition" >:: test_if_with_block_condition;
         "[parser] if then arithmetic" >:: test_if_then_arithmetic;
         "[parser] if then function call" >:: test_if_then_function_call;
         "[parser] if with complex condition" >:: test_if_with_complex_condition;
         "[parser] if with multiple comparisons"
         >:: test_if_with_multiple_comparisons;
         "[parser] if with equality" >:: test_if_with_equality;
         "[parser] if with inequality" >:: test_if_with_inequality;
         "[parser] sequential ifs" >:: test_sequential_ifs;
         "[parser] if after let" >:: test_if_after_let;
         "[parser] if with parenthesized condition"
         >:: test_if_with_parenthesized_condition;
         "[parser] if with parenthesized branches"
         >:: test_if_with_parenthesized_branches;
         "[parser] deeply nested ifs" >:: test_deeply_nested_ifs;
         "[parser] if chain else if pattern" >:: test_if_chain_else_if_pattern;
         "[parser] if with let in branches" >:: test_if_with_let_in_branches;
         "[parser] if without else in block" >:: test_if_without_else_in_block;
         "[parser] if with string branches" >:: test_if_with_string_branches;
         "[parser] if with boolean literals" >:: test_if_with_boolean_literals;
         "[parser] if result in let" >:: test_if_result_in_let;
         "[parser] match simple literal" >:: test_match_simple_literal;
         "[parser] match wildcard" >:: test_match_wildcard;
         "[parser] match variable pattern" >:: test_match_variable_pattern;
         "[parser] match nil pattern" >:: test_match_nil_pattern;
         "[parser] match cons pattern" >:: test_match_cons_pattern;
         "[parser] match nested cons pattern" >:: test_match_nested_cons_pattern;
         "[parser] match with guard" >:: test_match_with_guard;
         "[parser] match multiple guards" >:: test_match_multiple_guards;
         "[parser] match string literal" >:: test_match_string_literal;
         "[parser] match bool literal" >:: test_match_bool_literal;
         "[parser] match float literal" >:: test_match_float_literal;
         "[parser] match complex body" >:: test_match_complex_body;
         "[parser] match block body" >:: test_match_block_body;
         "[parser] match nested match" >:: test_match_nested_match;
         "[parser] match in function" >:: test_match_in_function;
         "[parser] match complex scrutinee" >:: test_match_complex_scrutinee;
         "[parser] match function call scrutinee"
         >:: test_match_function_call_scrutinee;
         "[parser] match parenthesized pattern"
         >:: test_match_parenthesized_pattern;
         "[parser] match cons with nil" >:: test_match_cons_with_nil;
         "[parser] match multiple cases" >:: test_match_multiple_cases;
         "[parser] match guard with equality" >:: test_match_guard_with_equality;
         "[parser] match guard with complex expr"
         >:: test_match_guard_with_complex_expr;
         "[parser] match result in let" >:: test_match_result_in_let;
         "[parser] match as function argument"
         >:: test_match_as_function_argument;
         "[parser] match sequential" >:: test_match_sequential;
         "[parser] match after let" >:: test_match_after_let;
         "[parser] match cons right associative"
         >:: test_match_cons_right_associative;
         "[parser] match guard with function call"
         >:: test_match_guard_with_function_call;
         "[parser] match literal then wildcard"
         >:: test_match_literal_then_wildcard;
         "[parser] match fibonacci style" >:: test_match_fibonacci_style;
         "[parser] match list length style" >:: test_match_list_length_style;
         "[parser] match with if in body" >:: test_match_with_if_in_body;
         "[parser] match exhaustive bool" >:: test_match_exhaustive_bool;
         "[parser] subtraction" >:: test_subtraction;
         "[parser] division" >:: test_division;
         "[parser] mixed arithmetic" >:: test_mixed_arithmetic;
         "[parser] function call no args" >:: test_function_call_no_args;
         "[parser] function call single arg" >:: test_function_call_single_arg;
         "[parser] function call multiple args"
         >:: test_function_call_multiple_args;
         "[parser] function call nested" >:: test_function_call_nested;
         "[parser] function call with arithmetic"
         >:: test_function_call_with_arithmetic;
         "[parser] chained function calls" >:: test_chained_function_calls;
         "[parser] block three exprs" >:: test_block_three_exprs;
         "[parser] nested blocks" >:: test_nested_blocks;
         "[parser] block with function call" >:: test_block_with_function_call;
         "[parser] let with arithmetic body" >:: test_let_with_arithmetic_body;
         "[parser] let with function call body"
         >:: test_let_with_function_call_body;
         "[parser] let with block body" >:: test_let_with_block_body;
         "[parser] sequential lets" >:: test_sequential_lets;
         "[parser] recursive function detection"
         >:: test_recursive_function_detection;
         "[parser] non recursive function" >:: test_non_recursive_function;
         "[parser] function with multiple return points"
         >:: test_function_with_multiple_return_points;
         "[parser] function returning function"
         >:: test_function_returning_function;
         "[parser] simple declaration" >:: test_adt_simple_declaration;
         "[parser] single variant" >:: test_adt_single_variant;
         "[parser] variant with fields" >:: test_adt_variant_with_fields;
         "[parser] result type" >:: test_adt_result_type;
         "[parser] multi field variant" >:: test_adt_multi_field_variant;
         "[parser] multiple declarations" >:: test_adt_multiple_declarations;
         "[parser] constructor expr" >:: test_adt_constructor_expr;
         "[parser] constructor no args" >:: test_adt_constructor_no_args;
         "[parser] constructor multiple args"
         >:: test_adt_constructor_multiple_args;
         "[parser] constructor nested" >:: test_adt_constructor_nested;
         "[parser] constructor in let" >:: test_adt_constructor_in_let;
         "[parser] constructor pattern" >:: test_adt_constructor_pattern;
         "[parser] constructor pattern multi field"
         >:: test_adt_constructor_pattern_multi_field;
         "[parser] constructor pattern nested"
         >:: test_adt_constructor_pattern_nested;
         "[parser] constructor pattern wildcard"
         >:: test_adt_constructor_pattern_with_wildcard;
         "[parser] list literal empty" >:: test_list_literal_empty;
         "[parser] list literal singleton" >:: test_list_literal_singleton;
         "[parser] list literal multi" >:: test_list_literal_multi;
         "[parser] list literal floats" >:: test_list_literal_floats;
         "[parser] list literal strings" >:: test_list_literal_strings;
         "[parser] list literal bools" >:: test_list_literal_bools;
         "[parser] list literal exprs" >:: test_list_literal_exprs;
         "[parser] list literal nested" >:: test_list_literal_nested;
         "[parser] list literal in let" >:: test_list_literal_in_let;
         "[parser] list literal as arg" >:: test_list_literal_as_arg;
         "[parser] cons simple" >:: test_cons_simple;
         "[parser] cons right associative" >:: test_cons_right_associative;
         "[parser] cons onto literal" >:: test_cons_onto_literal;
         "[parser] cons variable" >:: test_cons_variable;
         "[parser] cons in let" >:: test_cons_in_let;
         "[parser] type annotation list int" >:: test_type_annotation_list_int;
         "[parser] type annotation list float"
         >:: test_type_annotation_list_float;
         "[parser] type annotation list string"
         >:: test_type_annotation_list_string;
         "[parser] type annotation list bool" >:: test_type_annotation_list_bool;
         "[parser] type annotation function int to int"
         >:: test_type_annotation_function_int_to_int;
         "[parser] type annotation function multi param"
         >:: test_type_annotation_function_multi_param;
         "[parser] type annotation function no params"
         >:: test_type_annotation_function_no_params;
         "[parser] type annotation function returns string"
         >:: test_type_annotation_function_returns_string;
         "[parser] type annotation function returns bool"
         >:: test_type_annotation_function_returns_bool;
         "[parser] type annotation function takes list"
         >:: test_type_annotation_function_takes_list;
         "[parser] type annotation in param" >:: test_type_annotation_in_param;
         "[parser] type annotation list in return"
         >:: test_type_annotation_list_in_return;
         "[parser] type annotation function in param"
         >:: test_type_annotation_function_in_param;
         "[parser] type annotation function returns function"
         >:: test_type_annotation_function_returns_function;
         "[parser] error missing then" >:: test_parse_error_missing_then;
         "[parser] error missing body" >:: test_parse_error_missing_body;
         "[parser] error unclosed paren" >:: test_parse_error_unclosed_paren;
         "[parser] error unclosed brace" >:: test_parse_error_unclosed_brace;
         "[parser] error unclosed bracket" >:: test_parse_error_unclosed_bracket;
         "[parser] error trailing comma in list"
         >:: test_parse_error_trailing_comma_in_list;
         "[parser] error trailing comma in args"
         >:: test_parse_error_trailing_comma_in_args;
         "[parser] error missing arrow in match"
         >:: test_parse_error_missing_arrow_in_match;
         "[parser] error empty match" >:: test_parse_error_empty_match;
         "[parser] error let missing equals"
         >:: test_parse_error_let_missing_equals;
         "[parser] error let missing body" >:: test_parse_error_let_missing_body;
         "[parser] error double arrow" >:: test_parse_error_double_arrow;
         "[parser] error bare arrow" >:: test_parse_error_bare_arrow;
         "[parser] error match no pipe" >:: test_parse_error_match_no_pipe;
         "[parser] error unclosed match" >:: test_parse_error_unclosed_match;
         "[parser] error cons missing rhs" >:: test_parse_error_cons_missing_rhs;
         "[parser] error operator missing lhs"
         >:: test_parse_error_operator_missing_lhs;
         "[parser] error operator missing rhs"
         >:: test_parse_error_operator_missing_rhs;
         "[parser] error double colon in type"
         >:: test_parse_error_double_colon_in_type;
       ]
