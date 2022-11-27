open OUnit2
open Game
open Movable
open Camel
open Human
open Gui
open Item
open Pacmap
open State
open Bogue
open Main
open Tsdl
open Utils
open Random
module W = Widget
module L = Layout
module T = Trigger

let canvas = W.sdl_area ~w:800 ~h:800 ()
let sdl_area = W.get_sdl_area canvas

(*create a new map to run tests on*)
let map_1 = ref (gen_map 5 sdl_area)

(*test edge case where 0 is passed in (lowest value it can be)*)
let map_2 = ref (gen_map 0 sdl_area)

(*Create a camel to test the camel functions*)
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let camel_2 = Camel.("assets/images/camel-cartoon.png" |> init !map_2)
let human_2 = Human.("assets/images/human.png" |> init !map_2)

(*check that src is actually the same one*)
(* let _ = print_endline (string_of_int (fst (Human.size human_2)))*)
let camel_1_ref = ref camel_1
let human_1_ref = ref human_1
let initial_pos_camel = Camel.pos !camel_1_ref
let initial_pos_human = Human.pos !human_1_ref
let helper = Pacmap.camel_ctx !map_1
let convert_to_string i s = s ^ " " ^ (i |> string_of_int)
let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

let string_of_point (x, y) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let comp_points (x0, y0) (x1, y1) = x0 = x1 && y0 = y1

let helper_match = function
  | (x1, x2), (y1, y2), z ->
      print_endline (List.fold_right convert_to_string [ x1; x2; y1; y2; z ] "")

let empty_fun () = ()
let _ = Camel.move camel_1_ref map_1 (0, 0) empty_fun
let pos0 = initial_pos_camel |> to_canvas
let pos1 = Camel.pos !camel_1_ref |> to_canvas
let _ = Camel.move camel_1_ref map_1 (1, 0) empty_fun
let pos2 = Camel.pos !camel_1_ref |> to_canvas

(*shadowing old camel_1*)
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let camel_1_ref = ref camel_1
let _ = Camel.move camel_1_ref map_1 (0, 1) empty_fun
let pos3 = Camel.pos !camel_1_ref |> to_canvas
let pos0_human = initial_pos_human |> to_canvas
let pos1_human = Human.pos !human_1_ref |> to_canvas
let _ = Human.move human_1_ref map_1 (1, 0) empty_fun
let pos2_human = Human.pos !human_1_ref |> to_canvas
let _ = Human.move human_1_ref map_1 (0, 1) empty_fun
let pos3_human = Human.pos !human_1_ref |> to_canvas

let movable_tests =
  [
    ( " sample test 0: camel speed on canvas before moving should be the \
       initialized \n\
      \   speed of 2 "
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( "sample test 1: Checking no matter what map the camels are on that their \
       speed is at 2 this \n\
      \   tests the property of that speeds are identical throughout all maps \
       to ensure \n\
      \   fairness "
    >:: fun _ -> assert_equal (Camel.speed camel_1) (Camel.speed camel_2) );
    ( "sample test 2: camel position on canvas after move with dir = (0,0) \
       does not change the final position of the camel, this test ensures the \
       property \n\
      \       that after 'not moving' by moving but with a vector length of 0 \
       we are still at the same position\n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison"
    >:: fun _ -> assert_equal true (comp_points pos0 pos1) );
    ( "sample test 3: camel position on canvas after move with dir = (1,0) \
       will change (as long as there is no way in the direction (1,0) from the \
       current camel position), this test will check the property that after \
       moving \n\
       in (1,0) that we are expecting the camel to be at a different position \
       given the fact \n\
       there is an empty spot (prechecked) \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison"
    >:: fun _ -> assert_equal false (comp_points pos0 pos2) );
    ( "sample test 4: testing that after moving with dir = (1,0) with the \
       camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( "sample test 5: same as sample test 2, but testing positions directly. \
       Note: the Camel.move from sample test 2 has already changed position of \
       camel, so no need to call it again. \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison "
    >:: fun _ -> assert_equal true (pos0 |> add (1, 0) |> comp_points pos2) );
    ( "sample test 6: testing that after moving with dir = (0,1) with hte \
       camel on \n\
      \    the canvas that the position is no longer the same as the initial, \
       this is given \n\
      \    the fact there is an open path for the camel to move to that \
       direction, here initial means the camel's position before moving (1,0) \n\
      \            "
    >:: fun _ -> assert_equal false (comp_points pos0 pos3) );
    ( "sample test 7: same as sample test 6, but testing positions directly. \
       Note: the Camel.move from sample test 6 has already changed position of \
       camel, so no need to call it again. \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison "
    >:: fun _ -> assert_equal true (pos0 |> add (0, 1) |> comp_points pos3) );
    ( "sample test 8: testing that after moving with dir = (0,1) with the \
       camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 9: human speed on canvas before moving should be the \
       initialized \n\
      \   speed of 1 "
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( "sample test 10: human position on canvas after move with dir = (0,0) \
       does not change the final position of the human, this test ensures the \
       property \n\
      \       that after 'not moving' by moving but with a vector length of 0 \
       we are still at the same position\n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison"
    >:: fun _ -> assert_equal true (comp_points pos0_human pos1_human) );
    ( "sample test 11: human position on canvas after move with dir = (1,0) \
       will change (as long as there is no way in the direction (1,0) from the \
       current human position), this test will check the property that after \
       moving \n\
       in (1,0) that we are expecting the human to be at a different position \
       given the fact \n\
       there is an empty spot (prechecked) \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison"
    >:: fun _ -> assert_equal false (comp_points pos0_human pos2_human) );
    ( "sample test 12: same as sample test 11, but testing positions directly. \
       Note: the Human.move from sample test 11 has already changed position \
       of human, so no need to call it again. \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos0_human |> add (1, 0) |> comp_points pos2_human) );
    ( "sample test 13: testing that after moving with dir = (0,1) with the \
       human on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( "sample test 14: testing that after moving with dir = (0,1) with hte \
       human on \n\
      \    the canvas that the position is no longer the same as the initial, \
       this is given \n\
      \    the fact there is an open path for the human to move to that \
       direction, here initial means the human's position before moving (1,0) \n\
      \            "
    >:: fun _ -> assert_equal false (comp_points pos0_human pos3_human) );
    ( "sample test 15: The position of the human should have not changed \n\
      \    since there is no way for the human to move in that direction so \
       their \n\
      \    position should not have changed compared to as if they moved (1,0) \n\
      \       NOTE: coordinates on sdl_area are converted to coordinates on the\n\
      \           canvas before comparison "
    >:: fun _ -> assert_equal true (comp_points pos2_human pos3_human) );
    ( "sample test 16: testing that after moving with dir = (0,1) with the \
       human on \n\
       the canvas that the speed remains the same given the fact the camel has \
       not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
  ]

let gui_tests = []
let item_tests = []
let pacmap_tests = []
let state_tests = []
let tests = []
let suite = "test suite for Pac Caml" >::: List.flatten [ movable_tests ]
let _ = run_test_tt_main suite
