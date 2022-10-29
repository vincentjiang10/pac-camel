(** Representation of items in game.

    This module represents the data stored items, including its x and y
    position, as well as logic and effects to be executed upon trigger (when
    user passes upon a cell with item) *)

type t

val probability : t -> float
val period : t -> int
val src : t -> string
val effect : t -> unit
val animate : t -> unit
(* TODO (once main graphics loop is implemented): implement an animation
   function for all items *)

val coinsItem : unit -> t
val speedItem : unit -> t
val trajectoryItem : unit -> t
val sandItem : unit -> t
val phaseItem : unit -> t
val cactusItem : unit -> t