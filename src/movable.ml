open Pacmap

module type Movable = sig
  type t

  val pos : t -> int * int
  val speed : t -> int
  val src : t -> string
  val size : t -> int * int
  val move : t ref -> Pacmap.t -> int * int -> unit
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
end

module Camel : Movable = struct
  (* Two instances of camel state modification:
   * - on hitting camel item (valid_move)
   * - on item expiration (TBD)
   *)
  include MovableCommon
  (* type camel_state = { has_2x : bool } let state = ref { has_2x = false }*)

  let init map image =
    (* let default_state = { has_2x = false } in state := default_state;*)
    let pos, size, speed = camel_ctx map in
    { pos; size; src = image; speed }

  (* depending on the camel state, move may have different side effects on [t]
     and on [map]. For example, we may mutate [!t]'s position or speed with an
     item *)
  (* TODO: may need to take in a list of references (human references); Reason
     being a few items might affect both camels and humans *)
  let move t map (dir_x, dir_y) =
    let x, y = !t.pos in
    let p = (x + dir_x, y + dir_y) in
    let p_new, space = find_move map (x, y) p in
    update_pos !t p_new;
    match space with
    | Mass item -> ()
    | Empty -> ()
end

module Human : Movable = struct
  (* Possible instances of human state modification:
   * - Upon camel hitting certain camel items
   * - (possible) upon hitting human items
   *)
  include MovableCommon

  (* type human_state = { is_scared : bool }

     let state = ref { is_scared = false } *)
  let index = ref 1

  let init map image =
    (* let default_state = { is_scared = false } in state := default_state;*)
    incr index;
    (* take the mod 4 of index to get positioning of humans *)
    let pos, size, speed = human_ctx map (!index mod 4) in
    { pos; size; src = image; speed }

  (* depending on the human state, move may have different side effects on [t]
     and on [map] *)
  let move t map (dir_x, dir_y) =
    let x, y = !t.pos in
    let p = (x + dir_x, y + dir_y) in
    let p_new, item = find_move map (x, y) p in
    update_pos !t p_new
end
