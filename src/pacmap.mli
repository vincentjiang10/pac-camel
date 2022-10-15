(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

type t

val valid_move : t -> float * float -> bool
val start_pos : t -> float * float
