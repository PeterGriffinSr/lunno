open OUnit2

let () =
  run_test_tt_main
    ("lunno tests"
    >::: [
           Test_lexer.suite;
           Test_parser.suite;
           Test_typechecker.suite;
           Test_polymorphism.suite;
           Test_family.suite;
         ])
