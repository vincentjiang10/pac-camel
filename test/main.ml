(**Test plan: Description of parts that were manually tested, Ounit Tested, how
   they were tested and why this provides correctness to our programs and game.

   Parts of system manually tested via GUI and playing: We tested gui.mli,
   item.mli, pacmap.mli and state.mli via manual testing and platying, this is
   because this relies on the location at a given point of time in the game,
   what inputs the players does and how the state is updated when the keyboard
   actually detects an input as well as how the game should occur given some
   time passing (we don't want test cases to run for too long) therefore these
   modules were tested via multiple runs of the game, a possibility of different
   keyboard inputs and a wait time as we just let time passed and watch the
   scren to see what happens, to try and purposefully lose as well as to try and
   win.

   What modules were tested by OUnit and how test cases were developed: We
   tested Movable via OUnit testing as this was the only module we could test
   explicitly and rigidly without having to draw the map as we wanted the test
   cases to test the correctness of our program rigidly which couldn't be done
   correctly with other modules.

   We used mainly black box testing to ensure that all specifications were met,
   we followed the devious test cases trick as well as made sure test cases did
   only according to specs. We also had some white box testing to ensure that we
   were covering a lot of possible code via movements, camel generation, map
   generation, human generation, human movement, and finally how and where these
   humans and camel actually spawn since we expect more than one human on a map
   at a time. We thought above all possible directions and way a camel and human
   could move and we tested all possible allowable moves that the camel and
   human could move given the map generated. We made sure that these test cases
   were actually correct and followed specs and expectation again. Doing this
   black box and glass box testing allows us to ensure confidence in the
   correctness of our code. When looking at the code too via glass box testing
   we made sure that all possible branches were taken given the idea of the
   function.

   Why we believe our approach provides correctness to our game: The game's
   intention is to make sure that the camel is able to move at any given point
   of time, the humans are somewhat approaching the camel at any time once the
   timer has been passed for them to move, we also want to make sure that
   collisions between human and camels. We check that the score gets updated as
   the game is played via the widgets presented as well as how the game
   restarts. This was all done through possible edge cases while playing the
   game as well as letting time passes and see if there are any unusual effects.
   We also tested our code correctness by testing all possible ways a camel or
   human can move via the test file below as these are more rigorous to test
   given a map. We test that the speed is not affected after any given movement
   as well as positions are being updated given they are a possible way to move.
   Given all of this we have gone through all possible branches of things that
   could have occured throughout our game via manual and the actual test file.
   So this allows our game to have correctness in the way we have described, set
   it out to be and specified via documentation as we used black and glass box
   testing. *)

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
open Tsdl
open Main
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
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let camel_1_ref = ref camel_1
let _ = Camel.move camel_1_ref map_1 (0, -1) empty_fun
let pos4 = Camel.pos !camel_1_ref |> to_canvas
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let camel_1_ref = ref camel_1
let _ = Camel.move camel_1_ref map_1 (-1, 0) empty_fun
let pos5 = Camel.pos !camel_1_ref |> to_canvas
let pos0_human = initial_pos_human |> to_canvas
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let camel_1_ref = ref camel_1
let _ = Camel.move camel_1_ref map_1 (-1, -1) empty_fun
let pos6 = Camel.pos !camel_1_ref |> to_canvas
let _ = Camel.move camel_1_ref map_1 (1, 1) empty_fun
let _ = Camel.move camel_1_ref map_1 (1, -1) empty_fun
let pos7 = Camel.pos !camel_1_ref |> to_canvas
let pos1_human = Human.pos !human_1_ref |> to_canvas
let _ = Human.move human_1_ref map_1 (1, 0) empty_fun
let pos2_human = Human.pos !human_1_ref |> to_canvas
let _ = Human.move human_1_ref map_1 (0, 1) empty_fun
let pos3_human = Human.pos !human_1_ref |> to_canvas

(*shadowing old human_1*)
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1
let _ = Human.move human_1_ref map_1 (0, -1) empty_fun
let pos4_human = Human.pos !human_1_ref |> to_canvas
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1
let _ = Human.move human_1_ref map_1 (-1, 0) empty_fun
let pos5_human = Human.pos !human_1_ref |> to_canvas
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1

(*had to create multiple humans because of the way human_init was implemented
  needed a specific human that I want to test. *)
let pos_human_0 = Human.pos human_1 |> to_canvas
let _ = Human.move human_1_ref map_1 (-1, -1) empty_fun
let pos6_human = Human.pos !human_1_ref |> to_canvas
let _ = Human.move human_1_ref map_1 (-1, -1) empty_fun
let pos_human_1 = Human.pos human_1 |> to_canvas
let _ = Human.move human_1_ref map_1 (1, 1) empty_fun
let pos7_human = Human.pos !human_1_ref |> to_canvas
let human_1 = Human.("assets/images/human.png" |> init !map_1)
let human_1_ref = ref human_1
let pos_human_2 = Human.pos human_1 |> to_canvas
let _ = Human.move human_1_ref map_1 (1, -1) empty_fun
let pos8_human = Human.pos human_1 |> to_canvas
let _ = Human.move human_1_ref map_1 (-1, 1) empty_fun
let _ = Human.move human_1_ref map_1 (2, 0) empty_fun
let pos9_human = Human.pos human_1 |> to_canvas
let _ = Human.move human_1_ref map_1 (-2, 0) empty_fun
let _ = Human.move human_1_ref map_1 (0, 2) empty_fun
let pos10_human = Human.pos human_1 |> to_canvas
let camel_1 = Camel.("assets/images/camel-cartoon.png" |> init !map_1)
let camel_1_ref = ref camel_1
let pos8 = Camel.pos !camel_1_ref |> to_canvas
let _ = Camel.move camel_1_ref map_1 (2, 0) empty_fun
let pos9 = Camel.pos !camel_1_ref |> to_canvas
let _ = Camel.move camel_1_ref map_1 (-2, 0) empty_fun
let _ = Camel.move camel_1_ref map_1 (0, 2) empty_fun
let pos10 = Camel.pos !camel_1_ref |> to_canvas

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
    ( " sample test 17: Testing after moving with dir = (0,-1) with the human on\n\
      \  the canvas that the position is no longer the same as the initial, \n\
      \  this is given the fact that there is an open path for the human to \
       move to \n\
      \  that direction, here initial means the human's position before moving \
       anywhere. "
    >:: fun _ -> assert_equal false (comp_points pos4_human pos0_human) );
    ( "sample test 18: testing that after moving with dir = (0,1) with the \
       human on \n\
       the canvas that the speed remains the same given the fact the camel has \
       not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( " sample test 19: Testing after moving with dir = (0,-1) with the camel on\n\
      \  the canvas that the position is no longer the same as the initial, \n\
      \  this is given the fact that there is an open path for the camel to \
       move to \n\
      \  that direction, here initial means the camel's position before moving \
       anywhere. "
    >:: fun _ -> assert_equal false (comp_points pos4 pos0) );
    ( "sample test 20: same as sample test 19, but testing positions directly.\n\
      \   Note: the Camel.move from sample test 19 has already changed \
       position of camel, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ -> assert_equal true (pos0 |> add (0, -1) |> comp_points pos4) );
    ( "sample test 21: testing that after moving with dir = (0,-1) with the \
       camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 22: Testing after moving with dir = (-1,0) with the camel on\n\
      \  the canvas that the position is  the same as the initial, \n\
      \  this is given the fact that there is not an open path for the camel \
       to move to \n\
      \  that direction, here initial means the camel's position before moving \
       anywhere. "
    >:: fun _ -> assert_equal true (comp_points pos5 pos0) );
    ( " sample test 23: Testing after moving with dir = (-1,-1) with the camel \
       on \n\
      \    the canvas that the position is not the same as the initial, this \
       is given the \n\
      \    fact that there is an open path for the camel to move to in that \
       direction, \n\
      \    here initial means the came's position before moviing anywhere"
    >:: fun _ -> assert_equal false (comp_points pos6 pos0) );
    ( "sample test 24: same as sample test 23, but testing positions directly.\n\
      \   Note: the Camel.move from sample test 23 has already changed \
       position of camel, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ -> assert_equal true (pos0 |> add (-1, -1) |> comp_points pos6) );
    ( "sample test 25: testing that after moving with dir = (-1,-1) with the \
       camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 26: Testing after moving with dir = (-1,-1) with the human \
       on \n\
      \    the canvas that the position is not the same as the initial, this \
       is given the \n\
      \    fact that there is an open path for the human to move to in that \
       direction, \n\
      \    here initial means the came's position before moviing anywhere"
    >:: fun _ -> assert_equal false (comp_points pos6_human pos_human_0) );
    ( "sample test 27: same as sample test 26, but testing positions directly.\n\
      \   Note: the Human.move from sample test 26 has already changed \
       position of human, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos_human_0 |> add (-1, -1) |> comp_points pos6_human)
    );
    ( "sample test 28: testing that after moving with dir = (-1,-1) with the \
       human on \n\
       the canvas that the speed remains the same given the fact the camel has \
       not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( " sample test 29: Testing after moving with dir = (1,1) with the human on \n\
      \    the canvas that the position is not the same as the initial, this \
       is given the \n\
      \    fact that there is an open path for the human to move to in that \
       direction, \n\
      \    here initial means the came's position before moviing anywhere"
    >:: fun _ -> assert_equal false (comp_points pos7_human pos_human_1) );
    ( "sample test 30: same as sample test 29, but testing positions directly.\n\
      \   Note: the Human.move from sample test 29 has already changed \
       position of human, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos_human_1 |> add (1, 1) |> comp_points pos7_human) );
    ( "sample test 31: testing that after moving with dir = (1,1) with the \
       human on \n\
       the canvas that the speed remains the same given the fact the camel has \
       not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( " sample test 32: Testing after moving with dir = (1,-1) with the camel on \n\
      \    the canvas that the position is not the same as the initial, this \
       is given the \n\
      \    fact that there is an open path for the camel to move to in that \
       direction, \n\
      \    here initial means the came's position before moviing anywhere"
    >:: fun _ -> assert_equal false (comp_points pos7 pos0) );
    ( "sample test 33: same as sample test 32, but testing positions directly.\n\
      \   Note: the Camel.move from sample test 32 has already changed \
       position of camel, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ -> assert_equal true (pos0 |> add (1, -1) |> comp_points pos7) );
    ( "sample test 34: testing that after moving with dir = (1,-1) with the \
       camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 35: Testing after moving with dir = (1,-1) with the human on \n\
      \    the canvas that the position is the not same as the initial, this \
       is given the \n\
      \    fact that there is an open path for the human to move to in that \
       direction, \n\
      \    here initial means the came's position before moviing anywhere"
    >:: fun _ -> assert_equal false (comp_points pos8_human pos_human_2) );
    ( "sample test 36: same as sample test 35, but testing positions directly.\n\
      \   Note: the Human.move from sample test 35 has already changed \
       position of human, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos_human_2 |> add (1, -1) |> comp_points pos8_human)
    );
    ( "sample test 37: testing that after moving with dir = (1,-1) with the \
       human on \n\
       the canvas that the speed remains the same given the fact the camel has \
       not picked\n\
      \   up any power ups yet since humans can not modify their own speed \
       since \n\
      \      they are not the players"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( " sample test 38: Testing moving for some (x,y) where y = 0 and x>1 to \
       test \n\
      \  how moving with x >1 results with the move function, this moves the \
       camel \n\
      \  on the canvas, this is given the fact there is an open path for the \
       camel \n\
      \  to move 2 blocks over in that direction from the initial position, \n\
      \  here the initial position means the camel's position before moving \
       anywhere"
    >:: fun _ -> assert_equal false (comp_points pos8 pos9) );
    ( "sample test 39: same as sample test 38, but testing positions directly.\n\
      \   Note: the Camel.move from sample test 38 has already changed \
       position of camel, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ -> assert_equal true (pos8 |> add (2, 0) |> comp_points pos9) );
    ( "sample test 40: testing that moving with some (x,y) where x >1 but y = 0 \n\
      \    does not affect the speed of the camel, namely moving with dir = \
       (2,0) with the camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 41: Testing moving for some (x,y) where x = 0 and y>1 to \
       test \n\
      \  how moving with y >1 results with the move function, this moves the \
       camel \n\
      \  on the canvas, this is given the fact there is an open path for the \
       camel \n\
      \  to move 2 blocks over in that direction from the initial position, \n\
      \  here the initial position means the camel's position before moving \
       anywhere"
    >:: fun _ -> assert_equal false (comp_points pos8 pos10) );
    ( "sample test 42: same as sample test 41, but testing positions directly.\n\
      \   Note: the Camel.move from sample test 41 has already changed \
       position of camel, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ -> assert_equal true (pos8 |> add (0, 2) |> comp_points pos10) );
    ( "sample test 43: testing that moving with some (x,y) where y >1 but x = 0 \n\
      \    does not affect the speed of the camel, namely moving with dir = \
       (0,2) with the camel on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet"
    >:: fun _ -> assert_equal 2 (Camel.speed camel_1) );
    ( " sample test 44: Testing moving for some (x,y) where y = 0 and x>1 to \
       test \n\
      \  how moving with x >1 results with the move function, this moves the \
       human \n\
      \  on the canvas, this is given the fact there is an open path for the \
       human \n\
      \  to move 2 blocks over in that direction from the initial position, \n\
      \  here the initial position means the human's position before moving \
       anywhere"
    >:: fun _ -> assert_equal false (comp_points pos9_human pos_human_2) );
    ( "sample test 45: same as sample test 44, but testing positions directly.\n\
      \   Note: the Human.move from sample test 44 has already changed \
       position of human, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos_human_2 |> add (2, 0) |> comp_points pos9_human) );
    ( "sample test 46: testing that moving with some (x,y) where x >1 but y = 0 \n\
      \    does not affect the speed of the human, namely moving with dir = \
       (2,0) with the human on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet since a human is not able to change their own \
       speed \n\
      \      but the player/camel must pick up a powerup for this to occur"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
    ( " sample test 47: Testing moving for some (x,y) where x = 0 and y>1 to \
       test \n\
      \  how moving with y >1 results with the move function, this moves the \
       human \n\
      \  on the canvas, this is given the fact there is an open path for the \
       human \n\
      \  to move 2 blocks over in that direction from the initial position, \n\
      \  here the initial position means the human's position before moving \
       anywhere"
    >:: fun _ -> assert_equal false (comp_points pos10_human pos_human_2) );
    ( "sample test 48: same as sample test 47, but testing positions directly.\n\
      \   Note: the Human.move from sample test 47 has already changed \
       position of human, so no need to call it again. \n\
      \  NOTE: coordinates on sdl_area\n\
      \  are converted to coordinates on the\n\
      \  canvas before comparison "
    >:: fun _ ->
      assert_equal true (pos_human_2 |> add (0, 2) |> comp_points pos10_human)
    );
    ( "sample test 46: testing that moving with some (x,y) where y >1 but x = 0 \n\
      \    does not affect the speed of the human, namely moving with dir = \
       (0,2) with the human on \n\
      \   the canvas that the speed remains the same given the fact the camel \
       has not picked\n\
      \   up any power ups yet since a human is not able to change their own \
       speed \n\
      \      but the player/camel must pick up a powerup for this to occur"
    >:: fun _ -> assert_equal 1 (Human.speed human_1) );
  ]

let suite = "test suite for Pac Caml" >::: List.flatten [ movable_tests ]
let _ = run_test_tt_main suite
