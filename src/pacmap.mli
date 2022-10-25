(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

(** The abstract type of values representing pacmap *)
type t

(** [valid move t p] is true if moving to point [p] in pacmap [t] is valid.
    Updates game state. *)
val valid_move : t -> int * int -> bool

(** [camel_ctx t] is the starting point and size of the camel in pacmap [t] *)
val camel_ctx : t -> (int * int) * (int * int)

(** [gen_map s a] is a generated seeded random pacmap dependent on seed [s] and
    sdl_area [a]*)
val gen_map : int -> Bogue.Sdl_area.t -> t

(** [add_item t] adds a random item to map t *)
val add_item : t -> unit

(** [draw_map s t] draws [t] to sdl_area [s] *)
val draw_map : Bogue.Sdl_area.t -> t -> unit