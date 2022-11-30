open Pacmap
open Item
open State

module type Movable = sig
  type t

  val pos : t -> int * int
  val speed : t -> int
  val src : t -> string
  val size : t -> int * int
  val move : t ref -> Pacmap.t ref -> int * int -> (unit -> unit) -> unit
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

  let tween (x_from, y_from) (x_to, y_to) t render =
    let apply f (x, y) = (f x, f y) in
    let diff = (x_to - x_from, y_to - y_from) |> apply float_of_int in
    let numRenders = 100 in
    for i = 1 to numRenders do
      let scale = float_of_int i /. float_of_int numRenders in
      let x_diff, y_diff = diff |> apply (( *. ) scale) in
      (* rounds to the nearest int pair *)
      let p_update =
        (float_of_int x_from +. x_diff, float_of_int y_from +. y_diff)
        |> apply int_of_float
      in
      update_pos !t p_update;
      render ()
    done;
    update_pos !t (x_to, y_to)
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
  let move t map_ref dir render =
    let map = !map_ref in
    let p_curr = !t.pos in
    let p_new, space = find_move map p_curr dir in
    tween p_curr p_new t render;
    match space with
    | Mass item_ref -> begin
        let item = !item_ref in
        effect item;
        (* TODO: add possible changes due to items to game state (State.ml),
           camel, and humans *)
        match item_type item with
        | BigCoin -> ()
        | SmallCoin ->
            (* removes the coin at camel location in game board *)
            remove_item map_ref p_new
        | Coins -> ()
        | Speed -> ()
        | Traj -> ()
        | Sand -> ()
        | Phase -> ()
        | Cactus -> ()
        | Tele -> ()
        | Dim -> ()
        | Life -> ()
        | Time -> ()
      end
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
    (* take the mod 4 of index to get positioning of humans *)
    let pos, size, speed = human_ctx map (!index mod 4) in
    incr index;
    { pos; size; src = image; speed }

  (* depending on the human state, move may have different side effects on [t]
     and on [map] *)
  let move t map_ref dir render =
    let p_curr = !t.pos in
    let p_new, item_ref = find_move !map_ref p_curr dir in
    tween p_curr p_new t render
end
