(* Contains logic for the pac-camel *)
open Pacmap

type state = { has_2x : bool }

type t = {
  x : float;
  y : float;
  speed : float;
  src : string;
  state : state;
}

let get_x t = t.x
let get_y t = t.y

(* not finished yet; map is used to check if a move is valid *)
(* TODO @Vincent: make sure to implement wrap around *)
let move t (map : Pacmap.t) (x, y) =
  if valid_move map t.state (x, y) then t else { t with x; y }

let init map image =
  let pos = start_pos map in
  {
    x = fst pos;
    y = snd pos;
    src = image;
    speed = 1.;
    state = { has_2x = false };
  }
