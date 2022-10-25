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

let camel_texture = Tsdl_image.Image.load "assets/images/camel-cartoon.png"
let get_pos t = t.pos
let update_pos t p = t.pos <- p

(* not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t map (x, y) = ()
(* let flr f = f |> floor |> int_of_float in let modulo x y = let result = x mod
   y in if result >= 0 then result else result + y in let w, l = size map in let
   rem v = let v_flr = v |> floor in v -. v_flr in let x_int, y_int = (flr x |>
   modulo w, flr y |> modulo l) in if valid_move map (x_int, y_int) then
   update_pos t (float_of_int x_int +. rem x, float_of_int y_int +. rem y) *)

let scale (from_w, from_h) (to_w, to_h) p = p
(*TODO: take into account size of each block in map*)

let init map image =
  let win_size = (800, 800) in
  let scale = scale (size map) win_size in
  let pos = start_pos map |> scale in
  let size = (1, 1) |> scale in
  { pos; size; src = image; speed = 1; state = { has_2x = false } }

let get_src t = t.src