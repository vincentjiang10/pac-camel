(** Representation of static map data.

    This module represents the data stored in a map, including wall positions
    and spaces *)

(** The abstract type of values representing pacmap *)
type t

(** The type of values representing a space on the pacmap*)
type space =
  | Empty
  | Mass of Item.t ref

(** [find_move t p1 dir] is a pair with the updated point of attempting to move
    in direction [dir] from point [p1] in pacmap [t] along with the item at
    [p2]. Updates game state. *)
val find_move : t -> int * int -> int * int -> (int * int) * space

(** [camel_ctx t] is the starting point, size, and speed of the camel in pacmap
    [t] *)
val camel_ctx : t -> (int * int) * (int * int) * int

(** [human_init_pos t ind] is the initial position of a human depending on [t]
    and [ind]. The position is relative to the sdl_area. *)
val human_init_pos : t -> int -> int * int

(** [human_ctx t] is the starting point, size, and speed of a human in pacmap
    [t] *)
val human_ctx : t -> int -> (int * int) * (int * int) * int

(** [gen_map s a] is a generated seeded random pacmap dependent on seed [s] and
    sdl_area [a] Requires [t]>=0*)
val gen_map : int -> Bogue.Sdl_area.t -> t

(** [add_item t] adds a random item to a random position in map referenece [t] *)
val add_item : t ref -> unit

(** [remove_item t p] removes the item at camel location [p] in map reference
    [t] *)
val remove_item : t ref -> int * int -> unit

(** [check_item_expiration t_ref] removes expired items (items that have been on
    the board longer than their expected durations) *)
val check_item_expiration : t ref -> unit

(** [get_items t] is an association list containing the locations of items
    (relative to the sdl_area coordinate plane) and the reference of the items
    in map [t] *)
val get_items : t -> ((int * int) * Item.t ref) list

(** [animate_items l] mutates item refs in [l] according to their animate logic *)
val animate_items : Item.t ref list -> unit

(** [draw_map s t] draws [t] to sdl_area [s] *)
val draw_map : Bogue.Sdl_area.t -> t -> unit

(** [get_path_dir t src dst] is a direction unit vector as a point to proceed
    from [src] to [dst] without crossing a wall in map [t] *)
val get_path_dir : t -> int * int -> int * int -> int * int

(** [to_sdl_area p] converts a point [p] on the canvas/gameboard to a point on
    sdl_area *)
val to_sdl_area : int * int -> int * int

(** [to_canvas p] converts a point [p] on the sdl_area to a point on
    canvas/gameboard *)
val to_canvas : int * int -> int * int

(** [unit_size ()] is the size of the unit square of the current map *)
val unit_size : unit -> int * int