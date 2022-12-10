type game_state =
  | Inactive
  | Active
  | Pause
  | Win
  | Lose

type state

val init_state : unit -> state

(** [current_state state] is the game state of [state] *)
val current_state : state -> game_state

(** [change_state state] modifies the game state of [state] *)
val change_state : state -> game_state -> unit

(* [!state_end_time] is the time until the game round ends *)
val state_end_time : int ref

(* [!state_time] is a counter for the current time *)
val state_time : int ref

(* [!state_score] is the score counter for the current game *)
val state_score : int ref

(* [!state_lives] is the number of lives the camel has *)
val state_lives : int ref

(* [human_state] is the type containing possible human states *)
type human_state = {
  mutable scared : bool;
  mutable halfSpeed : bool;
  mutable doubleSpeed : bool;
}

(* [reset_state_human] resets human state *)
val reset_state_human : unit -> unit

(* [!state_human] is the current human state *)
val state_human : human_state

(* [camel_state] is the type containing possible camel states *)
type camel_state = {
  mutable doubleCoin : bool;
  mutable doubleSpeed : bool;
  mutable ignoreWalls : bool;
  mutable invincible : bool;
}

(* [reset_state_camel] resets camel state *)
val reset_state_camel : unit -> unit

(* [!state_camel] is the current camel state *)
val state_camel : camel_state
