(** Representation of movable objects in the game

    This module represents the data stored in a movable object, including its x
    and y position. *)

(** A [Movable] is a movable object in the game *)
module type Movable = sig
  (** The abstract type of values representing a movable object *)
  type t

  (** [pos t] is the location of the object [t] *)
  val pos : t -> int * int

  (** [speed t] is the speed of the object [t] that is in [0, 10] *)
  val speed : t -> int

  (** [src t] is the src image of the object [t] *)
  val src : t -> string

  (** [size t] is the size of the object [t] *)
  val size : t -> int * int

  (** [move t m dir f] updates the object [!t]'s location and renderes it on the
      game board using [f] after attempting to move in the unit direction [dir]
      in pacmap [m]. Example: if [!t]'s position is initially (12, 12) and a
      move attempt of [dir] = (1, 0) in pacmap [m] is successful, then update
      [!t]'s location to (13, 12). Also has item side effects: any item
      encountered by [t] may change the game state *)
  val move : t ref -> Pacmap.t -> int * int -> (unit -> unit) -> unit

  (** [init t s] is an the object with source image at [s] and dimensions,
      position, and speed depending on [t] *)
  val init : Pacmap.t -> string -> t
end

(** [Camel] is a module representing a movable camel object *)
module Camel : Movable

(** [Human] is a module representing a movable human object *)
module Human : Movable
