open Pacmap

module type Movable = sig
  type t

  val pos : t -> int * int
  val speed : t -> int
  val src : t -> string
  val size : t -> int * int
  val move : t -> Pacmap.t -> int * int -> unit
  val init : Pacmap.t -> string -> t
end

module MovableCommon = struct
  type t = {
    mutable pos : int * int;
    mutable size : int * int;
    mutable speed : int;
    src : string;
  }

  let pos t = t.pos
  let update_pos t p = t.pos <- p
  let speed t = t.speed
  let src t = t.src
  let size t = t.size

  let move t map (dir_x, dir_y) =
    let x, y = t.pos in
    let p = (x + dir_x, y + dir_y) in
    let p_new = find_move map (x, y) p in
    update_pos t p_new
end

module Camel : Movable = struct
  (* Two instances of camel state modification:
   * - on hitting camel item (valid_move)
   * - on item expiration (TBD)
   *)
  include MovableCommon

  type camel_state = { has_2x : bool }

  let state = ref { has_2x = false }

  let init map image =
    let default_state = { has_2x = false } in
    state := default_state;
    let pos, size = camel_ctx map in
    { pos; size; src = image; speed = fst size }
end

module Human = struct
  (* Possible instances of human state modification:
   * - Upon camel hitting certain camel items
   * - (possible) upon hitting human items
   *)
  include MovableCommon

  type human_state = { is_scared : bool }

  let state = ref { is_scared = false }
  let index = ref 0

  let init map image =
    let default_state = { is_scared = false } in
    state := default_state;
    index := !index + 1;
    (* take the mod 3 of index to get positioning of humans *)
    let pos, size = human_ctx map (!index mod 3) in
    { pos; size; src = image; speed = fst size }
end
