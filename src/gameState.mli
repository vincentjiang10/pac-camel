(** Representation of states in a game.

    This module represents the states the game can take on*)
open Movable

open Pacmap

type game_state =
  | Inactive
  | Initialize
  | Active
  | Pause
  | Win
  | Lose

type state

(** [init camel human_lst item_lst map] initialize a game state with at
    Inactive, with the given camel, huamns, and items on [map]*)
val init : Camel.t -> Human.t ref list -> Item.t list -> Pacmap.t -> state

(** [reset state camel human_lst item_lst map] reset the game with provided
    camel, huamns, and items on [map] *)
val reset :
  state ->
  Camel.t ->
  Camel.t ->
  Human.t ref list ->
  Item.t list ->
  Pacmap.t ->
  state

val current_state : state -> game_state
val next_state : state -> game_state -> unit
