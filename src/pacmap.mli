(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

(** The abstract type of values representing pacmap *)
type t

(** [valid move t c p] is true if moving camel [c] to point [p] in pacmap [t] is
    valid. Updates game state. *)
val valid_move : t -> float * float -> bool

(** [update_camel_state s p] returns the updated state if camel with state [s]
    moves to point [p] *)
val update_camel_state : 'a -> float * float -> 'a

(** [start_pos t] is the starting point of the camel in pacmap [t] *)
val start_pos : t -> float * float

(** [gen_map s] is a generated seeded random pacmap dependent on seed [s] *)
val gen_map : int -> t

(** [add_item t] adds a random item to map t *)
val add_item : t -> t

(** [draw_map s t] draws [t] to sdl_area [s] *)
val draw_map : Bogue.Sdl_area.t -> t -> unit