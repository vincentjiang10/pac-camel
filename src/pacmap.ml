open Random

type e = {
  bcoin : bool;
  scoin : bool;
}

type space =
  | Wall
  | Empty of e

type t = {
  data : space array array;
  start : float * float;
  size : int * int;
}

(* is [(x, y)] in [t] = type [e] *)
(* keep in mind that a camel should only move along the midline of each cell *)
let valid_move map (x, y) =
  let flr f = f |> floor |> int_of_float in
  let y = flr y in
  let x = flr x in
  if x < 0 || y < 0 || x >= fst map.size || y >= snd map.size then false
  else
    match map.data.(x).(y) with
    | Wall -> false
    | Empty _ -> true

let start_pos t = t.start

(* creates a pacmap with random size, say from 20 to 30 inclusive *)
let gen_map (seed : int) =
  let _ = init seed in
  let s = int 11 + 20 in
  let data = Array.make_matrix s s Wall in
  let data =
    Array.map
      (Array.map (fun _ ->
           if float 1. > 0.5 then Wall
           else Empty { bcoin = false; scoin = false }))
      data
  in
  { data; (* dependent on s*)
          start = (0., 0.); size = (s, s) }

let draw_wall sdl_area (x, y) (w_scale, h_scale) =
  let open Bogue in
  Sdl_area.fill_rectangle sdl_area
    ~color:
      Draw.(
        match grey with
        | r, g, b -> (r, g, b, 255))
    ~w:w_scale ~h:h_scale
    (x * w_scale, y * w_scale)

let draw_empty sdl_area e (x, y) (w_scale, h_scale) =
  let open Bogue in
  Sdl_area.fill_rectangle sdl_area
    ~color:
      Draw.(
        match cyan with
        | r, g, b -> (r, g, b, 100))
    ~w:w_scale ~h:h_scale
    (x * w_scale, y * w_scale)

let draw_map sdl_area (map : t) =
  let open Bogue in
  let w_to, h_to = Sdl_area.drawing_size sdl_area in
  let w_from, h_from = (fst map.size, snd map.size) in
  let scale = (w_to / w_from, h_to / h_from) in
  for x = 0 to w_from - 1 do
    for y = 0 to h_from - 1 do
      let p = (x, y) in
      match map.data.(x).(y) with
      | Wall -> draw_wall sdl_area p scale
      | Empty e -> draw_empty sdl_area e p scale
    done
  done
