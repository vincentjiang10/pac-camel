(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

(** The abstract type of values representing pacmap *)
type t

(** [size t] is the size of pacmap [t] as a pair = (width, height) *)
val size : t -> int * int

(** [valid move t p] is true if moving to point [p] in pacmap [t] is valid *)
val valid_move : t -> float * float -> bool

(** [start_pos t] is the starting point of the camel in pacmap [t] *)
val start_pos : t -> float * float

(** [gen_map s] is a generated seeded random pacmap dependent on seed [s] *)
val gen_map : int -> t

(* draw_map: called by main to draw original map *)
(* cover_cell: called by main when camel crosses a cell with coin -> draw white
   square on top of coin *)