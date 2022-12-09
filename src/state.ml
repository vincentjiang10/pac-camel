(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
type game_state =
  | Inactive
  | Active
  | Pause
  | Win
  | Lose

type state = game_state ref

let init_state () = ref Inactive
let change_state state game = state := game
let current_state st = !st

(* default: 1 minute *)
let state_end_time = ref 6000

(* Game round ends when state_time reaches state_end_time *)
let state_time = ref 0
let state_score = ref 0
let state_lives = ref 3

(* Human state *)

type human_state = {
  mutable scared : bool;
  mutable halfSpeed : bool;
  mutable goHome : bool;
}

let default_state_human = { scared = false; halfSpeed = false; goHome = false }
let state_human = default_state_human

let reset_state_human =
  state_human.scared <- default_state_human.scared;
  state_human.halfSpeed <- default_state_human.halfSpeed;
  state_human.goHome <- default_state_human.goHome

(* Camel state *)

type camel_state = {
  mutable doubleCoin : bool;
  mutable doubleSpeed : bool;
  mutable ignoreWalls : bool;
  mutable invincible : bool;
}

let default_state_camel =
  {
    doubleCoin = false;
    doubleSpeed = false;
    ignoreWalls = false;
    invincible = false;
  }

let state_camel = default_state_camel

let reset_state_camel =
  state_camel.doubleCoin <- default_state_camel.doubleCoin;
  state_camel.doubleSpeed <- default_state_camel.doubleSpeed;
  state_camel.ignoreWalls <- default_state_camel.ignoreWalls;
  state_camel.invincible <- default_state_camel.invincible