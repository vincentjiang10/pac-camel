open Pacmap

(* Two instances of camel state modification:
 *  - on hitting item (valid_move)
 *  - on item expiration (TBD)
 *)

type camel_state = { has_2x : bool }

type t = {
  x : float;
  y : float;
  speed : float;
  src : string;
  state : camel_state;
}

let get_x t = t.x
let get_y t = t.y

(* not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t map (x, y) =
  if valid_move map (x, y) then
    let state = update_camel_state t.state (x, y) in
    { t with x; y; state }
  else t

let init map image =
  let pos = start_pos map in
  {
    x = fst pos;
    y = snd pos;
    src = image;
    speed = 1.;
    state = { has_2x = false };
  }
