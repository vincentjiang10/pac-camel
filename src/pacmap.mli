(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

type t

val valid_move : t -> float * float -> bool
val start_pos : t -> float * float

(* gen_map: generates random map (may take in a seed arg for a seeded random
   map) *)
(* draw_map: called by main to draw original map *)
(* cover_cell: called by main when camel crosses a cell with coin -> draw white
   square on top of coin *)