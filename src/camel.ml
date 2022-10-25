open Pacmap
open Bogue
module L = Layout
module W = Widget

(* Two instances of camel state modification:
 *  - on hitting item (valid_move)
 *  - on item expiration (TBD)
 *)

type camel_state = { has_2x : bool }

type camel_area = {
  w : W.t;
  l : L.t;
  mutable a : Sdl_area.t;
}

type t = {
  mutable pos : int * int;
  mutable size : int * int;
  mutable speed : int;
  mutable state : camel_state;
  src : string;
}

let get_pos t = t.pos
let update_pos t p = t.pos <- p
let get_speed t = t.speed

(* scales the point from one dimension to another *)
let scale p_from p_to (x, y) =
  let to_float_coord (x, y) = (float_of_int x, float_of_int y) in
  let flr f = f |> floor |> int_of_float in
  let take_average p_from p_to =
    let x_from, y_from = to_float_coord p_from in
    let x_to, y_to = to_float_coord p_to in
    ((y_to /. y_from) +. (x_to /. x_from)) /. 2.
  in
  let factor = take_average p_from p_to in
  (factor *. float_of_int x |> flr, factor *. float_of_int y |> flr)

(* Not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t map (dir_x, dir_y) =
  let x, y = t.pos in
  update_pos t (x + dir_x, y + dir_y)
(* Does not work yet: need to transform camel canvas point to match game board
   coordinate grid let flr f = f |> floor |> int_of_float in let modulo x y =
   let result = x mod y in if result >= 0 then result else result + y in let w,
   l = size map in let rem v = let v_flr = v |> floor in v -. v_flr in let
   x_int, y_int = (flr x |> modulo w, flr y |> modulo l) in if valid_move map
   (x_int, y_int) then update_pos t (float_of_int x_int +. rem x, float_of_int
   y_int +. rem y)*)

let init map image =
  let win_size = (800, 800) in
  let scale = scale (size map) win_size in
  let pos = start_pos map |> scale in
  let size = (1, 1) |> scale in
  { pos; size; src = image; speed = 20; state = { has_2x = false } }

let get_src t = t.src