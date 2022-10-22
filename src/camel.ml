open Pacmap

(* Two instances of camel state modification:
 *  - on hitting item (valid_move)
 *  - on item expiration (TBD)
 *)

type camel_state = { has_2x : bool }

type t = {
  mutable x : int;
  mutable y : int;
  mutable speed : int;
  mutable state : camel_state;
  src : string;
}

let get_x t = t.x
let get_y t = t.y
let get_pos t = (t.x, t.y)
let update_x t x = t.x <- x
let update_y t y = t.y <- y

let update_pos t (x, y) =
  update_x t x;
  update_y t y

(* scales the point from one dimension to another *)
let scale (from_w, from_h) (to_w, to_h) p = p

(* Not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t map (x, y) = ()
(* Does not work yet: need to transform camel canvas point to match game board
   coordinate grid let flr f = f |> floor |> int_of_float in let modulo x y =
   let result = x mod y in if result >= 0 then result else result + y in let w,
   l = size map in let rem v = let v_flr = v |> floor in v -. v_flr in let
   x_int, y_int = (flr x |> modulo w, flr y |> modulo l) in if valid_move map
   (x_int, y_int) then update_pos t (float_of_int x_int +. rem x, float_of_int
   y_int +. rem y)*)

let init map image =
  let win_size = (800, 800) in
  let pos = start_pos map |> scale (size map) win_size in
  {
    x = fst pos;
    y = snd pos;
    src = image;
    speed = 1;
    state = { has_2x = false };
  }
