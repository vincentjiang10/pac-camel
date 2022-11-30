(** Representation of states in a game.

    This module represents the states the game can take on*)
open Movable

open Pacmap

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

type state

(** [init camel human_lst item_lst map] initialize a game state with at
    Inactive, with the given camel, huamns, and items on [map]*)
val init :
  Camel.t ref ->
  Human.t ref list ref ->
  Item.t list ref ->
  Pacmap.t ref ->
  state

(** [reset state camel human_lst item_lst map] reset the game with provided
    camel, huamns, and items on [map] *)
val reset : state -> unit

(** [current_state state] is the game state of [state] *)
val current_state : state -> game_state

(** [change_state state] modifies the game state of [state] *)
val change_state : state -> game_state -> unit

(** [update state event] updates the game state according to the event*)
val update : state -> event -> unit

(** score *)
val score : state -> int -> unit