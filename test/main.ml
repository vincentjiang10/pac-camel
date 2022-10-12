open OUnit2

let tests = []
let suite = "test suite for [file_name]" >::: tests
let _ = run_test_tt_main suite
