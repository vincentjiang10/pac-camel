open Random

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
  size : int * int;
}

let size t = t.size

(* is [(x, y)] in [t] = type [e] *)
(* keep in mind that a camel should only move along the midline of each cell *)
let valid_move (t : t) ((x, y) : float * float) =
  let flr f = f |> floor |> int_of_float in
  let y = flr y in
  let x = flr x in
  if x < 0 || y < 0 || x >= fst t.size || y >= snd t.size then false
  else
    match x |> List.nth (y |> List.nth t.data) with
    | Wall _ -> false
    | Empty _ -> true

let start_pos t = t.start

(* creates a pacmap with random size, say from 20 to 30 inclusive *)
let gen_map (seed : int) =
  let _ = init seed in
  let _ = print_endline (string_of_int (int 10)) in
  let _ = print_endline (string_of_int (int 10)) in
  let _ = print_endline (string_of_int (int 10)) in
  let _ = print_endline (string_of_int (int 10)) in
  let _ = print_endline (string_of_int (int 10)) in
  {
    data = [ [] ];
    start = (0., 0.);
    size =
      (let s = int 11 + 20 in
       (s, s));
  }
