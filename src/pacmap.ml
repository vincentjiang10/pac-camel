open Random

(* TODO @Vincent: add that type e of type Item.t*)
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

(* creates a pacmap with random size, say from 20 to 40 inclusive *)
(* TODO @Vincent: update gen_map function *)
let gen_map (seed : int) =
  let _ = init seed in
  let s = int 21 + 20 in
  let data = Array.make_matrix s s Wall in
  let data =
    Array.map
      (Array.map (fun _ ->
           if float 1. > 0.5 then Wall
           else Empty { bcoin = float 10. > 5.; scoin = float 20. > 10. }))
      data
  in
  { (* dependent on s*) data; start = (0., 0.); size = (s, s) }

let color_of_rgb c a =
  match c with
  | r, g, b -> (r, g, b, a)

let draw_circle sdl_area c r loc =
  Bogue.Sdl_area.fill_circle sdl_area ~color:c ~radius:r loc

let draw_rect sdl_area c w h loc =
  Bogue.Sdl_area.fill_rectangle sdl_area ~color:c ~w ~h loc

(* TODO @Vincent: make sure to enable random map color generation *)

let draw_wall map sdl_area (x, y) (shift_x, shift_y) (w, h) =
  let module D = Bogue.Draw in
  let grey = color_of_rgb D.dark_grey 255 in
  let w_2, h_2 = (w / 2, h / 2) in
  let x_0, y_0 = ((x * w) + shift_x, (y * h) + shift_y) in
  draw_circle sdl_area grey
    ((w_2 + h_2) / 4) (* take average *)
    (x_0 + w_2, y_0 + h_2);
  let is_wall = function
    | Wall -> true
    | Empty _ -> false
  in
  let { data; size; _ } = map in
  let x_max, y_max = size in
  let top, right, bottom, left =
    ( y != 0 && is_wall data.(x).(y - 1),
      x < x_max - 1 && is_wall data.(x + 1).(y),
      y < y_max - 1 && is_wall data.(x).(y + 1),
      x != 0 && is_wall data.(x - 1).(y) )
  in
  let w_4, h_4 = (w / 4, h / 4) in
  let w_2, h_2 = (w_2 + 1, h_2 + 1) in
  let draw_grey_rect = draw_rect sdl_area grey in
  if top then draw_grey_rect w_2 h_2 (x_0 + w_4, y_0);
  if right then draw_grey_rect w_2 h_2 (x_0 + w_2, y_0 + h_4);
  if bottom then draw_grey_rect w_2 h_2 (x_0 + w_4, y_0 + h_2);
  if left then draw_grey_rect w_2 h_2 (x_0, y_0 + h_4)

let draw_empty e sdl_area (x, y) (shift_x, shift_y) (w, h) =
  let module D = Bogue.Draw in
  (* draw background *)
  draw_rect sdl_area (color_of_rgb D.cyan 100) w h
    ((x * w) + shift_x, (y * h) + shift_y);
  (* draw small coin *)
  if e.scoin then ();
  (* draw big coin *)
  if e.bcoin then ();
  (* draw boundary *)
  draw_rect sdl_area
    (color_of_rgb D.dark_grey 100)
    (w + 2) (h + 2)
    ((x * w) + shift_x - 1, (y * h) + shift_y - 1)

let draw_map sdl_area (map : t) =
  let w_to, h_to = Bogue.Sdl_area.drawing_size sdl_area in
  let w_from, h_from = map.size in
  let scale = (w_to / w_from, h_to / h_from) in
  let shift = (w_to mod w_from / 2, h_to mod h_from / 2) in
  for x = 0 to w_from - 1 do
    for y = 0 to h_from - 1 do
      let p = (x, y) in
      match map.data.(x).(y) with
      | Wall ->
          (* swap drawing order to disable color layering *)
          draw_wall map sdl_area p shift scale;
          draw_empty { bcoin = false; scoin = false } sdl_area p shift scale
      | Empty e -> draw_empty e sdl_area p shift scale
    done
  done
