(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
open Movable

open Tsdl
open Sdl
open Item

type camel = Camel.t
type human_state = Human.t

type event =
  | Start
  | PauseEvent
  | Reset
  | Score of int
  | Camelmove of string

type game_state =
  | Inactive
  | Active
  | Pause
  | Win
  | Lose

type state = {
  mutable game : game_state;
  camel : camel ref;
  mutable lives : int;
  mutable score : int;
  human : human_state ref list ref;
  items : Item.t list ref;
  map : Pacmap.t ref;
}

(* initialize a state *)
let init camel human items map =
  { game = Inactive; camel; human; lives = 3; score = 0; items; map }

(* make change to current state *)
let reset state =
  state.lives <- 3;
  state.score <- 0

let change_state state game = state.game <- game
let score state inc = state.score <- state.score + inc

let update state event =
  match (state.game, event) with
  | Inactive, Start -> change_state state Active
  | Active, PauseEvent -> change_state state Pause
  | Active, _ when state.lives = 0 -> change_state state Lose
  | Active, Score i -> score state i
  | Pause, Start -> change_state state Active
  | _, _ -> ()

(* access information *)
let current_state st = st.game
