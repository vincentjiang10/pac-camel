(* Contains logic for the pac-camel *)
open Pacmap

type t = {
  x : float;
  y : float;
  src : string;
}

let get_x t = t.x
let get_y t = t.y

(* not finished yet; map is used to check if a move is valid *)
let move t (map : Pacmap.t) (x, y) =
  if valid_move map (x, y) then t else { t with x; y }

let init map image =
  let pos = start_pos map in
  { x = fst pos; y = snd pos; src = image }
