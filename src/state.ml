(* Contains game state logic *)

(** Representation of states in a game.

    This module represents the states the game can take on*)
type game_state =
  | Inactive
  | Active
  | Pause
  | Win
  | Lose

type state = {
  mutable game : game_state;
  mutable lives : int;
  mutable score : int;
}

let init_state () = { game = Inactive; lives = 3; score = 0 }

(* make change to current state *)
let reset state =
  state.lives <- 3;
  state.score <- 0

let change_state state game = state.game <- game
let current_state st = st.game

(* default: 1 minute *)
let state_end_time = ref 6000

(* Game round ends when state_time reaches state_end_time *)
let state_time = ref 0

(* TODO @Vincent: fix issue where coins are generated outside of map? *)
let state_score = ref 0
let state_human_scared = ref false