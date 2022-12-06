type game_state =
  | Inactive
  | Active
  | Pause
  | Win
  | Lose

type state

val init_state : unit -> state

(** [reset state camel human_lst item_lst map] reset the game with provided
    camel, huamns, and items on [map] *)
val reset : state -> unit

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

(* [!state_human_scared] is either true or false *)
val state_human_scared : bool ref
