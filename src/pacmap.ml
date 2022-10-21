open Random

(* NOTE: the drawing of items will not be on sdl_area but on the window
   renderer *)
(* A function written will be called that takes in map t and renders the items
   with their respective animations *)
type e =
  | Empty
  | Item of Item.t

type space =
  | Wall
  | Floor of e

type t = {
  data : space array array;
  start : float * float;
  size : int * int;
}

(* Note: a camel should only move along the midline of each cell *)
let valid_move map (x, y) =
  let flr f = f |> floor |> int_of_float in
  let y = flr y in
  let x = flr x in
  if x < 0 || y < 0 || x >= fst map.size || y >= snd map.size then false
  else
    match map.data.(x).(y) with
    | Wall -> false
    | Floor _ -> true

let valid_xy s (x, y) = min x y >= 0 && max x y < s

(* TODO: @Vincent, if (x,y) is on a cell with item, clear it -> set to Floor
   Empty + Update game state before clearing depending on item and
   camel_state *)
let update_camel_state c (x, y) = c
let start_pos t = t.start

module Point = struct
  type t = int * int

  let compare = compare
end

module PointSet = Set.Make (Point)

(* wall boundary: a set of points (x, y) denoting the boundary of the valid
   space to draw walls. It is *)
let wb = ref PointSet.empty
let wb_remove (x, y) = wb := PointSet.remove (x, y) !wb
let wb_add (x, y) = wb := PointSet.add (x, y) !wb

type wb_edit_type =
  | Add
  | Remove

let wall_check data s (x, y) =
  let is_wall (x, y) = valid_xy s (x, y) && data.(x).(y) = Wall in
  if is_wall (x, y - 1) then false
  else if is_wall (x + 1, y - 1) then false
  else if is_wall (x + 1, y) then false
  else if is_wall (x + 1, y + 1) then false
  else if is_wall (x, y + 1) then false
  else if is_wall (x - 1, y + 1) then false
  else if is_wall (x - 1, y) then false
  else if is_wall (x - 1, y - 1) then false
  else true

let rec wb_edit w_e_t data s (dir_x, dir_y) (x, y) len =
  if len != 0 && valid_xy s (x, y) then begin
    if w_e_t = Remove then wb_remove (x, y)
    else if w_e_t = Add then
      if wall_check data s (x, y) && min x y >= 2 && max x y <= s - 3 then
        wb_add (x, y);
    wb_edit w_e_t data s (dir_x, dir_y) (x + dir_x, y + dir_y) (len - 1)
  end
  else (x - dir_x, y - dir_y)

(* Draw a line segment of walls of length len from (x, y) in direction dir and
   returns the end point of that segment*)
let rec draw_walls data s (dir_x, dir_y) (x, y) len =
  if len != 0 && valid_xy s (x, y) then begin
    data.(x).(y) <- Wall;
    draw_walls data s (dir_x, dir_y) (x + dir_x, y + dir_y) (len - 1)
  end
  else (x - dir_x, y - dir_y)

(* Takes in an empty map data and sets up human starting zone in the center *)
let start_map data s =
  let mid = s / 2 in
  let walls = draw_walls data s in
  (* starting "room" containing humans *)
  let p = walls (1, 0) (mid + 1, mid - 1) 2 in
  let p = walls (0, 1) p 3 in
  let _ = walls (-1, 0) p 3 in

  (* generate bump *)
  let rec gen_bump (dir_x, dir_y) (x, y) (l1, l2) =
    let p_add =
      wb_edit Add data s (dir_x, dir_y)
        (x + (2 * (dir_x - dir_y)), y + (2 * (dir_x + dir_y)))
        l1
    in
    let p_add = wb_edit Add data s (dir_y, -dir_x) p_add (l2 + 4) in
    let _ = wb_edit Add data s (-dir_x, -dir_y) p_add l1 in
    let p_remove =
      wb_edit Remove data s (dir_x, dir_y)
        (x + (dir_x - dir_y), y + (dir_x + dir_y))
        l1
    in
    let p_remove = wb_edit Remove data s (dir_y, -dir_x) p_remove (l2 + 2) in
    let _ = wb_edit Remove data s (-dir_x, -dir_y) p_remove l1 in
    let p = walls (dir_x, dir_y) (x, y) l1 in
    let p = walls (dir_y, -dir_x) p l2 in
    let p_x, p_y = walls (-dir_x, -dir_y) p l1 in
    let prob = float 1. in
    let thres = 10 in
    let p_skip = (p_x + (2 * dir_y), p_y - (2 * dir_x)) in
    let p_skip_x, p_skip_y = p_skip in
    if prob > 0.5 && (dir_y * p_skip_x) + (-dir_x * p_skip_y) + thres <= s - 1
    then gen_bump (dir_x, dir_y) p_skip (3 + int 3, 3 + int 3)
    else
      let len = 3 + int 3 in
      let _ =
        wb_edit Add data s (dir_y, -dir_x)
          (p_skip_x + (2 * dir_x), p_skip_y + (2 * dir_y))
          len
      in
      walls (dir_y, -dir_x) p_skip len
  in

  (* generate boundary *)
  let rec gen_boundary (dir_x, dir_y) (x_i, y_i) (x_f, y_f) =
    let l1 = 2 + int 2 in
    let l2 = 4 + int 2 in
    let thres = 5 in
    if x_i > x_f || y_i > y_f then ()
    else if
      (float 1. > if dir_x = 1 then 0.9 else 0.95)
      && x_i >= dir_x * (mid + thres)
      && y_i >= dir_y * thres
      (* thres to not generate bumps near corners of boundary *)
      && x_i + (dir_x * (l2 + thres)) <= x_f
      && y_i + (dir_y * (l2 + thres)) <= y_f
    then
      let p = gen_bump (-dir_y, dir_x) (x_i, y_i) (l1, l2) in
      gen_boundary (dir_x, dir_y) p (x_f, y_f)
    else begin
      data.(x_i).(y_i) <- Wall;
      let p_x, p_y = (x_i - (2 * dir_y), y_i + (2 * dir_x)) in
      if min p_x p_y >= 2 && max p_x p_y <= s - 3 then wb_add (p_x, p_y);
      gen_boundary (dir_x, dir_y) (x_i + dir_x, y_i + dir_y) (x_f, y_f)
    end
  in

  (* populate rest of wall boundary *)
  let wb_edit_add = wb_edit Add data s in
  let p = wb_edit_add (0, 1) (mid, 3) (mid - 5) in
  let p = wb_edit_add (1, 0) p 5 in
  let _ = wb_edit_add (0, 1) p 7 in

  (* drawing the boundary *)

  (* top half boundary *)
  gen_boundary (1, 0) (mid, 0) (s - 1, 0);
  (* reflect over x axis to get bottom half boundary *)
  let mirror_down_x data s =
    for x = mid to s - 1 do
      for y = mid + 3 to s - 1 do
        data.(x).(y) <- data.(x).(s - 1 - y);
        if PointSet.mem (x, s - 1 - y) !wb then wb_add (x, y)
      done
    done
  in
  mirror_down_x data s;
  (* right boundary *)
  gen_boundary (0, 1) (s - 1, 0) (s - 1, s - 1);
  ()

let test_wb data = PointSet.iter (fun (x, y) -> data.(x).(y) <- Wall) !wb

type shape_type =
  | I (* I shape *)
  | L (* L shape *)
  | T (* T shape *)
  | C (* C shape *)
  | E (* E shape *)
  | H (* H shape *)
  | U (* U shape *)

(* Takes in a starting map and populates it with tetris blocks representing
   paths *)
let populate_map data s =
  let shape_types = [ I; L; T; C; E; H ] in
  let shape_types = Array.of_list shape_types in
  let boundary_walk (x, y) len = (x, y) in
  (* generates a tetris-like wall with most points in [wb] starting at (x,y) a
     point in [wb] *)
  let rec gen_pieces (x, y) =
    if PointSet.is_empty !wb then ();
    (* TODO: change 0 to random index *)
    (* TODO: see if you can implement specific shapes *)
    let st = Array.get shape_types 0 in
    let len = 2 + int 3 in
    match st with
    | _ ->
        let p = boundary_walk (x, y) len in
        gen_pieces p
  in
  (* gen_pieces (PointSet.choose !wb); *)
  (* adding initial items to map *)
  for x = 0 to s - 1 do
    for y = 0 to s - 1 do
      if data.(x).(y) = Floor Empty then
        data.(x) (* TODO @Vincent: run probability of item appearing on floor *).(
        y) <- Floor Empty
    done
  done

(* Copies the right half of data over the y-axis to the left *)
let mirror_left_y data s =
  let mid = s / 2 in
  for x = 0 to mid - 1 do
    for y = 0 to s - 1 do
      data.(x).(y) <- data.(s - 1 - x).(y)
    done
  done

(* Creates a pacmap with random odd size *)
(* TODO @Vincent: update gen_map function *)
let gen_map (seed : int) =
  let _ = init seed in
  let s = (int 15 * 2) + 31 in
  let data = Array.make_matrix s s (Floor Empty) in
  wb := PointSet.empty;
  start_map data s;
  populate_map data s;
  test_wb data;
  mirror_left_y data s;
  { data; start = (0., 0.); size = (s, s) }

let add_item map = map

let color_of_rgb c a =
  match c with
  | r, g, b -> (r, g, b, a)

let draw_circle sdl_area c r loc =
  Bogue.Sdl_area.fill_circle sdl_area ~color:c ~radius:r loc

let draw_rect sdl_area c w h loc =
  Bogue.Sdl_area.fill_rectangle sdl_area ~color:c ~w ~h loc

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
    | Floor _ -> false
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

let draw_floor c sdl_area (x, y) (shift_x, shift_y) (w, h) =
  let module D = Bogue.Draw in
  (* draw background *)
  draw_rect sdl_area c w h ((x * w) + shift_x, (y * h) + shift_y);
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
  let c = (int 255, int 255, int 255, 80) in
  for x = 0 to w_from - 1 do
    for y = 0 to h_from - 1 do
      let p = (x, y) in
      match map.data.(x).(y) with
      | Wall ->
          (* swap drawing order to disable color layering *)
          draw_wall map sdl_area p shift scale;
          draw_floor c sdl_area p shift scale
      | Floor _ -> draw_floor c sdl_area p shift scale
    done
  done
