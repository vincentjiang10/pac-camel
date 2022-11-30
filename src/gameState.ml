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
  | Active
  | Pause
  | Win
  | Lose

type state = {
  mutable game : game_state;
  mutable camel : camel ref;
  mutable human : human_state ref list ref;
  mutable items : Item.t list ref;
  mutable map : Pacmap.t ref;
}

(* initialize a state *)
let init camel human items map = { game = Inactive; camel; human; items; map }

(* make change to current state *)
let change_state state game = state.game <- game
let change_camel state camel = state.camel <- camel
let change_human state human = state.human <- human
let change_map state map = state.map <- map
let change_items state item = state.items <- item

let reset state camel human items map =
  (* change_state state game; *)
  change_camel state camel;
  change_human state human;
  change_map state map;
  change_items state items

(* check current game state *)
let is_inactive state = state.game = Inactive
let is_active state = state.game = Active
let is_pause state = state.game = Pause
let is_win state = state.game = Win
let is_lose state = state.game = Lose

(* access information *)
let current_state st = st.game
(* let get_camel_surface state = Tsdl_image.Image.load (Camel.src
   (!state.camel))

   let get_human_surface state = List.map (fun x -> Tsdl_image.Image.load
   (Human.src !x)) !state.human *)
