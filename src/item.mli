(** Representation of items in game.

    This module represents the data stored items, including its x and y
    position, as well as logic and effects to be executed upon trigger (when
    user passes upon a cell with item) *)

(** [t] represents an item in a game *)
type t

type item =
  | BigCoin
  | SmallCoin
  | Coins
  | Speed
  | Sand
  | Phase
  | Cactus
  | Life
  | Time
  | Invincible

(** [probability t] is the probability of [t] showing up in a game *)
val probability : t -> float

(** [size t] is the size of [t] *)
val size : t -> int * int

(** [startTime t] is the time that the item is created *)
val startTime : t -> int

(** [duration t] will output the duration of how long [t] will last on the map *)
val duration : t -> int

(** [src t] is the image link of [t] *)
val src : t -> string

(** [effect t] is the effect of [t]. Note: can modify states in [state] *)
val effect : t -> unit

(** [animate t] animates item by mutating its contents (width, height, etc) *)
val animate : t ref -> unit

(** [flip t] gets the flip instruction for [t] *)
val flip : t -> Tsdl.Sdl.flip

(** [change_flip t_ref] changes [!t_ref]'s flip *)
val change_flip : t ref -> unit

(** [shift t] is the amount to shift the item by *)
val shift : t -> int * int

(** [itemType t] is the item type of t *)
val item_type : t -> item

(** [initItems (w,h)] sets up *)
val init_items : int * int -> unit

(** [genRandItem ()] is either a random item or [None]. Note for testing: use a
    while loop or other methods to simulate multiple [genRandItem ()] calls and
    see if the distribution matches that of each items probability *)
val gen_rand_item : unit -> t ref option
