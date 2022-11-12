(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
open Movable

open Item

type camel_state = Camel.t
type human_state = Human.t

type game_state =
  | Inactive
  | Initialize
  | Active
  | Pause
  | Win
  | Lose

type state = {
  mutable game : game_state;
  mutable camel : camel_state;
  mutable human : human_state list;
  mutable items : Item.t list;
}

(** [init c h i] initializes a new game with c*)
let init c h i = { game = Inactive; camel = c; human = h; items = i }

let current_state state : state = state
let camel_pos state = Camel.pos state.camel
let human_pos state = List.map (fun human -> Human.pos human) state.human

let next_state sCurr event =
  match sCurr with
  | Inactive -> () (*if key press detected, go to active*)
  | Initialize -> () (*next state is active *)
  | Active ->
      () (* if .. to pause, else if win then end, else if dies then end*)
  | Pause -> () (* if resume, Active*)
  | Win | Lose -> ()

let update sCurr =
  match sCurr with
  | Inactive -> ()
  | Initialize -> ()
  | Active -> () (*update camel and human position*)
  | Pause -> ()
  | Win | Lose -> ()
