(** Representation of camel in game.

    This module represents the data stored in a camel, including its x and y
    position. *)

(** The abstract type of values representing a camel *)
type t

val get_x : t -> float
val get_y : t -> float
val move : t -> Pacmap.t -> float * float -> t
val init : Pacmap.t -> string -> t
