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
  mutable x : float;
  mutable y : float;
  mutable speed : float;
  mutable state : camel_state;
  mutable area : camel_area;
  src : string;
}

let camel_texture = Tsdl_image.Image.load "assets/images/camel-cartoon.png"
let get_x t = t.x
let get_y t = t.y
let update_x t x = t.x <- x
let update_y t y = t.y <- y

let update_area t x y =
  L.setx t.area.l x;
  L.sety t.area.l y;
  t.area.a <- W.get_sdl_area t.area.w

let update_pos t (x, y) =
  update_x t x;
  update_y t y;
  update_area t (int_of_float x) (int_of_float y)

(* not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t map (x, y) =
  let flr f = f |> floor |> int_of_float in
  let modulo x y =
    let result = x mod y in
    if result >= 0 then result else result + y
  in
  let w, l = size map in
  let rem v =
    let v_flr = v |> floor in
    v -. v_flr
  in
  let x_int, y_int = (flr x |> modulo w, flr y |> modulo l) in
  if valid_move map (x_int, y_int) then
    update_pos t (float_of_int x_int +. rem x, float_of_int y_int +. rem y)

(*TODO: take into account size of each block in map*)
let init map image =
  let pos = start_pos map in
  let wid = W.sdl_area ~w:20 ~h:20 () in
  let lay =
    L.resident ~w:20 ~h:20
      ~x:(int_of_float (fst pos))
      ~y:(int_of_float (snd pos))
      wid
  in
  let a = W.get_sdl_area wid in
  let camel_a = { w = wid; l = lay; a } in
  {
    x = fst pos;
    y = snd pos;
    src = image;
    speed = 1.;
    state = { has_2x = false };
    area = camel_a;
  }

let get_src t = t.src
let get_layout t = t.area.l
let get_area t = t.area.a