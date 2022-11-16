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
open Sdl
module W = Widget
module L = Layout
module T = Trigger

let canvas = W.sdl_area ~w:800 ~h:800 ()
let sdl_area = W.get_sdl_area canvas

(*create a new map to run tests on*)
let map_1 = gen_map 5 sdl_area

(*test edge case where 0 is passed in (lowest value it can be)*)
let map_2 = gen_map 0 sdl_area

(*Create a camel to test the camel functions*)
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init map_1)
let human_1 = Human.("assets/images/human.png" |> init map_1)
let camel_2 = Camel.("assets/images/camel-cartoon.png" |> init map_2)
let human_2 = Human.("assets/images/human.png" |> init map_2)

(*check that src is actually the same one*)
(* let _ = print_endline (string_of_int (fst (Human.size human_2)))*)
let camel_1_ref = ref camel_1
let human_1_ref = ref human_1

let x, y =
  Camel.move camel_1_ref map_1 (0, 0);
  Camel.pos !camel_1_ref

let initial_pos_camel = Camel.pos !camel_1_ref
let initial_pos_human = Human.pos !human_1_ref
let _ = ()

let movable_tests =
  [
    ( "Testing that the camel has the same source as creation" >:: fun _ ->
      assert_equal "assets/images/camel-cartoon.png" (Camel.src camel_1) );
    ( " Testing that the camel has a size of (19,19) when initially created so \
       that it is ensured that the camel stays on the map \n\
      \    at any given point at time"
    >:: fun _ -> assert_equal (19, 19) (Camel.size camel_1) );
    ( "Testing that the camel has a speed of 19 intially to make sure that there \n\
      \    isn't an underlying bug with any future powerups of the camel's \
       speed changing "
    >:: fun _ -> assert_equal 19 (Camel.speed camel_1) );
    ( " Testing that a human has the same source as passed in when created and \
       doesn't collide \n\
      \    with the source image of the camel"
    >:: fun _ -> assert_equal "assets/images/human.png" (Human.src human_1) );
    ( " Testing that a human has a size of (19,19) intially to make sure that \
       the intial size \n\
      \    is within a range that can fit within a map "
    >:: fun _ -> assert_equal (19, 19) (Human.size human_1) );
    ( " Testing that a human has a speed of 19 initially to make sure that \
       there isn't \n\
      \   a bug that is generated when there are two Movable objects in the \
       map at the same time \n\
      \   that might affect the speed to change "
    >:: fun _ -> assert_equal 19 (Human.speed human_1) );
    ( "Testing that the initial pos is a valid tuple that is on the map, given \
       we want it within the area of the sdl\n\
      \  "
    >:: fun _ -> assert_equal (569, 611) (Camel.pos camel_1) );
    ( "Testing that the initial pos is a valid tuple that is on the map, given \
       we \n\
      \  want it within the area of the sdl"
    >:: fun _ -> assert_equal (470, 470) (Human.pos human_1) );
    ( " Testing that the start position for a differently generated map \
       implies that \n\
      \    the camel will start at a different position"
    >:: fun _ -> assert_equal (166, 299) (Camel.pos camel_2) );
    ( " Testing that the start position for a differently generated map \
       implies that the \n\
      \    human will start at a different position"
    >:: fun _ -> assert_equal (546, 527) (Human.pos human_2) );
    ( "Testing that no matter what map that the speed of the human will be the \
       same \n\
      \    in order to ensure fairness"
    >:: fun _ -> assert_equal 19 (Human.speed human_2) );
    ( "Testing that no matter what map that the speed of the camel will be the \
       same \n\
      \    in order to ensure fairness"
    >:: fun _ -> assert_equal 19 (Camel.speed camel_2) );
    ( "Testing that the size will be the same on a different map to ensure \
       that the sizes\n\
      \    are scaled to fit on the map given the same sdl_area  "
    >:: fun _ -> assert_equal (19, 19) (Human.size human_2) );
    ( "Testing that the size will be the same on a different map to ensure \
       that the sizes\n\
      \    are scaled to fit on the map given the same sdl_area  "
    >:: fun _ -> assert_equal (19, 19) (Camel.size camel_2) );
    ( "Testing that the position changes after moving the camel's ref version \
       given map_1 \n\
      \    and moving it once in the [0,0] direction updates the position to \
       the same \n\
      \      one it was previously at"
    >:: fun _ ->
      assert_equal (569, 611)
        (Camel.move camel_1_ref map_1 (0, 0);
         Camel.pos !camel_1_ref) );
    ( "Testing that the position changes after moving the human's ref version \
       given map_1 \n\
      \    and moving it once in the [0,0] direction updates the position to \
       the same \n\
      \      one it was previously at"
    >:: fun _ ->
      assert_equal (470, 470)
        (Human.move human_1_ref map_1 (0, 0);
         Human.pos !human_1_ref) );
    ( "Testing that the position changes after moving the camel's ref version \
       given map_1 \n\
      \    and moving it once in the [50,1] direction updates the position to \
       the same \n\
      \      one it was previously at"
    >:: fun _ ->
      assert_equal (603, 603)
        (Camel.move camel_1_ref map_1 (50, 1);
         Camel.pos !camel_1_ref) );
    ( "Property testing that after moving the camel that it should no longer \
       be the same \n\
      \    position as the initial position ebfore moving, this allows us to \
       ensure that \n\
      \    after moving [x,y] for any x in the naturals and y in the naturals \
       that it is not [xi,yi] \n\
      \    where xi is the initial x position and yi is the initial y position"
    >:: fun _ ->
      Camel.move camel_1_ref map_1 (103, 50);
      assert_equal true (initial_pos_camel <> Camel.pos !camel_1_ref) );
    ( "Testing that the position changes after moving the human's ref version \
       given map_1 \n\
      \    and moving it once in the [50,1] direction updates the position to \
       the same \n\
      \      one it was previously at"
    >:: fun _ ->
      assert_equal (470, 508)
        (Human.move human_1_ref map_1 (0, 50);
         Human.pos !human_1_ref) );
    ( "Property testing that after moving the human that it should no longer \
       be the same \n\
      \    position as the initial position ebfore moving, this allows us to \
       ensure that \n\
      \    after moving [x,y] for any x in the naturals and y in the naturals \
       that it is not [xi,yi] \n\
      \    where xi is the initial x position and yi is the initial y position"
    >:: fun _ ->
      Human.move human_1_ref map_1 (103, 50);
      assert_equal true (initial_pos_human <> Human.pos !human_1_ref) );
  ]

let gui_tests = []
let item_tests = []
let pacmap_tests = []
let state_tests = []
let tests = []
let suite = "test suite for Pac Caml" >::: List.flatten [ movable_tests ]
let _ = run_test_tt_main suite
