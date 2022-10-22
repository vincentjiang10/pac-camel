(** Representation of camel in game.

    This module represents the data stored in a camel, including its x and y
    position. *)

(** The abstract type of values representing a camel *)
type t

val get_pos : t -> int * int

(** [move t m p] is the camel's state containing its location after attempting
    to move to [p] *)
val move : t -> Pacmap.t -> float * float -> unit

(** [init t s] is a camel with source image at [s] and dimensions and position
    depending on [t] *)
val init : Pacmap.t -> string -> t
