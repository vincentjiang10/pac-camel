open Pacmap
open Bogue
module L = Layout
module W = Widget

(* Two instances of camel state modification:
 *  - on hitting item (valid_move)
 *  - on item expiration (TBD)
 *)

type camel_state = { has_2x : bool }

type t = {
  mutable pos : int * int;
  mutable size : int * int;
  mutable speed : int;
  mutable state : camel_state;
  src : string;
}

let pos t = t.pos
let update_pos t p = t.pos <- p
let speed t = t.speed
let src t = t.src
let size t = t.size

(* TODO @Vincent: make sure to implement wrap around *)
let move t map (dir_x, dir_y) =
  let x, y = t.pos in
  let p = (x + dir_x, y + dir_y) in
  let p_new = find_move map (x, y) p in
  update_pos t p_new

let init map image =
  let pos, size = camel_ctx map in
  { pos; size; src = image; speed = fst size; state = { has_2x = false } }
