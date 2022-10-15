open Coin
(* Contains map logic *)

type w = {
  x : float;
  y : float;
  src : string;
}

type e = {
  x : float;
  y : float;
  bcoin : bool;
  scoin : bool;
}

type space =
  | Wall of w
  | Empty of e

type t = {
  data : space list list;
  start : float * float;
}

let valid_move (t : t) (x, y) = true
let start_pos t = t.start