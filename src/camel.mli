(** Representation of camel in game.

    This module represents the data stored in a camel, including its x and y
    position. *)

(** The abstract type of values representing a camel *)
type t

(** [get_pos t] is the location of camel [t] *)
val get_pos : t -> int * int

(** [get_spped t] is the speed of camel [t] *)
val get_speed : t -> int

(** [move t m p] updates a camel [t]'s location after attempting to move in the
    direction and magnitude of [p] in pacmap [m]. Example: if [t]'s position is
    initially (12, 12) and a move attempt of [p] = (1, 0) in pacmap [m] is
    successful, then update [t]'s location to (13, 12) *)
val move : t -> Pacmap.t -> int * int -> unit

(** [init t s] is a camel with source image at [s] and dimensions and position
    depending on [t] *)
val init : Pacmap.t -> string -> t
