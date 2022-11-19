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
  | Pause
  | Camelmove of string

type game_state =
  | Inactive
  | Initialize
  | Active
  | Pause
  | Win
  | Lose

type state = {
  mutable game : game_state;
  mutable camel : camel;
  mutable human : human_state ref list;
  mutable items : Item.t list;
  mutable map : Pacmap.t;
}

(** [init c h i] initializes a new game with c*)
let init c h i m = { game = Active; camel = c; human = h; items = i; map = m }

let next_state state sNext = state.game <- sNext
let change_camel state new_camel = state.camel <- new_camel
let change_human state new_humans = state.human <- new_humans
let change_items state new_items = state.items <- new_items
let change_map state new_map = state.map <- new_map

let reset state new_camel new_humans new_items new_map =
  next_state state Active;
  change_camel state new_camel;
  change_human state new_humans;
  change_items state new_items;
  change_map state new_map

let current_state state : game_state = state.game
let camel_pos state = Camel.pos state.camel
let camel_texture state = Tsdl_image.Image.load (Camel.src state.camel)
let human_pos state = List.map (fun human -> Human.pos !human) state.human
