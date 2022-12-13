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
(*TODO: add a widget for this, endtime - current time*)

(* Game round ends when state_time reaches state_end_time *)
let state_time = ref 0
let state_score = ref 0
let state_num_coins = ref 0
let state_lives = ref 3

(* Human state *)

type human_state = {
  mutable scared : bool;
  mutable halfSpeed : bool;
  mutable doubleSpeed : bool;
}

let state_human = { scared = false; halfSpeed = false; doubleSpeed = false }

let reset_state_human () =
  state_human.scared <- false;
  state_human.halfSpeed <- false;
  state_human.doubleSpeed <- false

(* Camel state *)

type camel_state = {
  mutable doubleCoin : bool;
  mutable doubleSpeed : bool;
  mutable ignoreWalls : bool;
  mutable invincible : bool;
}

let state_camel =
  {
    doubleCoin = false;
    doubleSpeed = false;
    ignoreWalls = false;
    invincible = false;
  }

let reset_state_camel () =
  state_camel.doubleCoin <- false;
  state_camel.doubleSpeed <- false;
  state_camel.ignoreWalls <- false;
  state_camel.invincible <- false