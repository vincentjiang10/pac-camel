(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
let state_inactive = ref true

let state_active = ref false
let state_pause = ref false
let state_continue = ref false
let state_escape = ref false
let state_complete = ref false
let state_time = ref 0
let state_num_coins = ref 0