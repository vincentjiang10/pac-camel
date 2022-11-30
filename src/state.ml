(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
let state_inactive = ref true

let state_active = ref false
let state_pause = ref false
let state_continue = ref false
let state_escape = ref false
let state_complete = ref false

(* default: 1 minute *)
let state_end_time = ref 6000

(* Game round ends when state_time reaches state_end_time *)
let state_time = ref 0

(* TODO @Vincent: fix issue where coins are generated outside of map? *)
let state_score = ref 0
let state_human_scared = ref false