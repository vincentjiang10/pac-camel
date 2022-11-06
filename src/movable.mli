(** Representation of movable objects in the game

    This module represents the data stored in a movable object, including its x
    and y position. *)

(** A [Movable] is a movable object in the game *)
module type Movable = sig
  (** The abstract type of values representing a movable object *)
  type t

  (** [pos t] is the location of the object [t] *)
  val pos : t -> int * int

  (** [speed t] is the speed of the object [t] *)
  val speed : t -> int

  (** [src t] is the src image of the object [t] *)
  val src : t -> string

  (** [size t] is the size of the object [t] *)
  val size : t -> int * int

  (** [move t m p] updates a the object [t]'s location after attempting to move
      in the direction and magnitude of [p] in pacmap [m]. Example: if [t]'s
      position is initially (12, 12) and a move attempt of [p] = (1, 0) in
      pacmap [m] is successful, then update [t]'s location to (13, 12) *)
  val move : t -> Pacmap.t -> int * int -> unit

  (** [init t s] is a the object with source image at [s] and dimensions and
      position depending on [t] *)
  val init : Pacmap.t -> string -> t
end

(** [Camel] is a module representing a movable camel object *)
module Camel : Movable

(** [Human] is a module representing a movable human object *)
module Human : Movable
