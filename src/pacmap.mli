(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

(** The abstract type of values representing pacmap *)
type t

(** The type of values representing a space on the pacmap*)
type space =
  | Empty
  | Mass of Item.t

(** [find_move t p1 p2] is a pair with the updated point of attempting to move
    to point [p2] from point [p1] in pacmap [t] along with the item at [p2].
    Updates game state. *)
val find_move : t -> int * int -> int * int -> (int * int) * space

(** [camel_ctx t] is the starting point, size, and speed of the camel in pacmap
    [t] *)
val camel_ctx : t -> (int * int) * (int * int) * int

(** [human_ctx t] is the starting point, size, and speed of a human in pacmap
    [t] *)
val human_ctx : t -> int -> (int * int) * (int * int) * int

(** [gen_map s a] is a generated seeded random pacmap dependent on seed [s] and
    sdl_area [a] Requires [t]>=0*)
val gen_map : int -> Bogue.Sdl_area.t -> t

(** [add_item t] adds a random item to map t *)
val add_item : t -> unit

(** [draw_map s t] draws [t] to sdl_area [s] *)
val draw_map : Bogue.Sdl_area.t -> t -> unit

(** [get_path_dir t src dst] is a direction unit vector as a point to proceed
    from [src] to [dst] without crossing a wall in map [t] *)
val get_path_dir : t -> int * int -> int * int -> int * int