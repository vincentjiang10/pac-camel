open OUnit2
open Game
open Camel
open Gui
open Human
open Item
open Pacmap
open State

(*create a new map to run tests on*)
let map_1 = gen_map 5

(*test edge case where 0 is passed in (lowest value it can be)*)
let map_2 = gen_map 0
let camel_tests = []
let gui_tests = []
let human_tests = []
let item_tests = []
let pacmap_tests = []
let state_tests = []
let tests = []
let suite = "test suite for Pac Caml" >::: List.flatten []
let _ = run_test_tt_main suite
