open Pacmap

(* Two instances of camel state modification:
 *  - on hitting item (valid_move)
 *  - on item expiration (TBD)
 *)

type camel_state = { has_2x : bool }

type t = {
  mutable x : float;
  mutable y : float;
  mutable speed : float;
  mutable state : camel_state;
  src : string;
}

let get_x t = t.x
let get_y t = t.y
let update_x t x = t.x <- x
let update_y t y = t.y <- y

let update_pos t (x, y) =
  update_x t x;
  update_y t y

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

let init map image =
  let pos = start_pos map in
  {
    x = fst pos;
    y = snd pos;
    src = image;
    speed = 1.;
    state = { has_2x = false };
  }

let get_src t = t.src
let get_src t = t.src
