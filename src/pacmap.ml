open Random
open Item

(* NOTE: the drawing of items will not be on sdl_area but on the window
   renderer *)
(* A function written will be called that takes in map t and renders the items
   with their respective animations *)
type space =
  | Empty
  | Mass of Item.t

type cell =
  | Wall
  | Floor of space

type t = {
  data : cell array array;
  start : int * int;
  size : int * int;
}

let scale_x = ref 0
let scale_y = ref 0
let shift_x = ref 0
let shift_y = ref 0

(* Converts a point on the canvas/gameboard to a point on sdl_area *)
let to_sdl_area (x, y) = ((x * !scale_x) + !shift_x, (y * !scale_y) + !shift_y)

(* Converts a point on the sdl_area to a point on canvas/gameboard *)
let to_canvas (x, y) = ((x - !shift_x) / !scale_x, (y - !shift_y) / !scale_y)

(* TODO: @Vincent, if (x,y) is on a cell with item, clear it -> set to Floor
   Empty + Update game state before clearing depending on item and
   camel_state *)

let find_move map p_from p_new =
  let size = fst map.size in
  let bound v = if v < 0 then v + size else if v >= size then v - size else v in
  let x, y = to_canvas p_new in
  let x, y = (bound x, bound y) in
  match map.data.(x).(y) with
  | Wall -> (p_from, Empty)
  | Floor space -> (to_sdl_area (x, y), space)

let valid_xy s (x, y) = min x y >= 0 && max x y < s
let camel_ctx t = (t.start, (!scale_x, !scale_y))

let human_ctx t ind =
  let w, h = t.size in
  (* set initial position of humans to the center of map *)
  (((w / 2) - 2 + ind, h / 2) |> to_sdl_area, (!scale_x, !scale_y))

module Point = struct
  type t = int * int

  let compare = compare
end

(** A [PointSet] is an unordered collection of type [Point.t]. *)
module PointSet = Set.Make (Point)

(* Wall set: a set of all points (x, y) representing a wall in pacmap *)
let wall_set = ref PointSet.empty

(* Wall boundary: a set of points (x, y) denoting the boundary of the valid cell
   to draw walls. *)
let wall_boundary = ref PointSet.empty
let add p s = s := PointSet.add p !s

let add_wall data (x, y) =
  data.(x).(y) <- Wall;
  add (x, y) wall_set

let remove p s = s := PointSet.remove p !s
let empty s = s := PointSet.empty

(* Add a line segment of walls of length len from (x, y) in direction dir and
   returns the end point of that segment*)
let rec add_walls data s (dir_x, dir_y) (x, y) len =
  if len != 0 && valid_xy s (x, y) then begin
    add_wall data (x, y);
    add_walls data s (dir_x, dir_y) (x + dir_x, y + dir_y) (len - 1)
  end
  else (x - dir_x, y - dir_y)

let dist (x_0, y_0) (x_1, y_1) =
  let xDiff = x_1 - x_0 in
  let yDiff = y_1 - y_0 in
  (xDiff * xDiff) + (yDiff * yDiff)

let update_wb s =
  let mid = s / 2 in
  (* check whether [p] is in wall_boundary *)
  let in_wb (x, y) =
    let req1 = ref true in
    let req2 = ref false in
    for x_p = x - 2 to x + 2 do
      for y_p = y - 2 to y + 2 do
        let dist = dist (x, y) (x_p, y_p) in
        (* ensure all neighbors 1 away are not in [wall_set] *)
        (* ensure a neighbor 2 away is in [wall_set] *)
        if dist < 4 then begin
          if PointSet.mem (x_p, y_p) !wall_set then req1 := false
        end
        else if PointSet.mem (x_p, y_p) !wall_set then req2 := true
      done
    done;
    !req1 && !req2
  in
  (* reset wall boundary *)
  empty wall_boundary;
  for x = mid to s - 1 do
    for y = 0 to s - 1 do
      if in_wb (x, y) then add (x, y) wall_boundary
    done
  done

(* Takes in an empty map data and sets up starting zone in the center *)
let start_map data s =
  let mid = s / 2 in
  let walls = add_walls data s in
  (* starting "room" containing humans *)
  let p = walls (1, 0) (mid + 1, mid - 1) 2 in
  let p = walls (0, 1) p 3 in
  walls (-1, 0) p 3 |> ignore;

  (* generate bump *)
  let rec gen_bump (dir_x, dir_y) (x, y) (l1, l2) =
    let p = walls (dir_x, dir_y) (x, y) l1 in
    let p = walls (dir_y, -dir_x) p l2 in
    let p_x, p_y = walls (-dir_x, -dir_y) p l1 in
    let prob = float 1. in
    let thres = 10 in
    let p_skip = (p_x + (2 * dir_y), p_y - (2 * dir_x)) in
    let p_skip_x, p_skip_y = p_skip in
    if prob > 0.7 && (dir_y * p_skip_x) + (-dir_x * p_skip_y) + thres <= s - 1
    then gen_bump (dir_x, dir_y) p_skip (3 + int 3, 3 + int 3)
    else walls (dir_y, -dir_x) p_skip (4 + int 3)
  in

  (* generate boundary *)
  let rec gen_boundary (dir_x, dir_y) (x_i, y_i) (x_f, y_f) =
    let l1 = 2 + int 2 in
    let l2 = 4 + int 2 in
    let thres = 5 in
    if x_i > x_f || y_i > y_f then ()
    else if
      (float 1. > if dir_x = 1 then 0.8 else 0.9)
      && x_i >= dir_x * (mid + thres)
      && y_i >= dir_y * thres
      (* thres to not generate bumps near corners of boundary *)
      && x_i + (dir_x * (l2 + thres)) <= x_f
      && y_i + (dir_y * (l2 + thres)) <= y_f
    then
      let p = gen_bump (-dir_y, dir_x) (x_i, y_i) (l1, l2) in
      gen_boundary (dir_x, dir_y) p (x_f, y_f)
    else begin
      add_wall data (x_i, y_i);
      gen_boundary (dir_x, dir_y) (x_i + dir_x, y_i + dir_y) (x_f, y_f)
    end
  in

  (* drawing the boundary *)

  (* top half boundary *)
  gen_boundary (1, 0) (mid, 0) (s - 1, 0);
  (* reflect over x axis to get bottom half boundary *)
  let mirror_down_x data s =
    for x = mid to s - 1 do
      for y = mid + 3 to s - 1 do
        data.(x).(y) <- data.(x).(s - 1 - y);
        if data.(x).(y) = Wall then add (x, y) wall_set
      done
    done
  in
  mirror_down_x data s;
  (* right boundary *)
  gen_boundary (0, 1) (s - 1, 0) (s - 1, s - 1);
  ()

let test_wb data =
  PointSet.iter (fun (x, y) -> data.(x).(y) <- Wall) !wall_boundary

(* Takes in a starting map and populates it with tetris blocks representing
   walls *)
let populate_map data s =
  let boundary_walk () =
    let wb_list = PointSet.elements !wall_boundary in
    let rec add_n_walls wall_points len =
      if len == 0 then ()
      else
        match wall_points with
        | h :: t ->
            add_wall data h;
            add_n_walls t (len - 1)
        | [] -> ()
    in
    let gen_wall_points () =
      let elt = List.nth wb_list (int (List.length wb_list)) in
      let dist = dist elt in
      (* sort wb in order or increasing distance away from random element
         [elt] *)
      let wall_points =
        List.sort
          (fun (x_0, y_0) (x_1, y_1) -> dist (x_0, y_0) - dist (x_1, y_1))
          wb_list
      in
      let len = 2 + (s / 20) + int 6 in
      add_n_walls wall_points len
    in
    (* generate more walls to improve map generation efficiency *)
    for i = 1 to 3 do
      gen_wall_points ()
    done;
    update_wb s
  in
  (* generates a tetris-like wall with most points in [wb] starting at (x,y) a
     point in [wb] *)
  let rec gen_pieces () =
    if PointSet.is_empty !wall_boundary then ()
    else begin
      boundary_walk ();
      gen_pieces ()
    end
  in
  gen_pieces ();
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

(* [!paths.(src_in).(dest_in)] is direction as a tuple representing a move to
   proceed from point [src = (x_s, y_s)] to point [dst = (x_d, y_d)], where
   [src_in = x_s * s + y_s], [dst_in = x_d * s + y_d], and [s = Array.length
   !paths]. A path is a list of points [(dir_x, dir_y)] *)
let paths = ref [||]

(* Precomputing paths on gen_map *)
let precompute_paths data =
  let len = Array.length data in
  paths := Array.make_matrix (len * len) (len * len) (0, 0)

let get_path_dir src dst =
  let (x_src, y_src), dst = (src |> to_canvas, dst |> to_canvas) in
  (* greedy choice substitute *)
  let dirs = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
  List.fold_left
    (* TODO: add randomness to direction *)
      (fun (x_dir, y_dir) (x_dir', y_dir') ->
      if
        dist (x_src + x_dir', y_src + y_dir') dst
        < dist (x_src + x_dir, y_src + y_dir) dst
      then (x_dir', y_dir')
      else (x_dir, y_dir))
    (0, 0) dirs
(* let to_ind (x, y) = (x * Array.length !paths) + y in !paths.(src |>
   to_ind).(dst |> to_ind) *)

(* Creates a pacmap with random odd size *)
let gen_map seed sdl_area =
  init seed;
  let s = (2 * int 20) + 30 in
  let data = Array.make_matrix s s (Floor Empty) in
  empty wall_set;
  empty wall_boundary;
  start_map data s;
  update_wb s;
  populate_map data s;
  test_wb data;
  mirror_left_y data s;
  precompute_paths data;
  let w_to, h_to = Bogue.Sdl_area.drawing_size sdl_area in
  scale_x := w_to / s;
  scale_y := h_to / s;
  shift_x := w_to mod s / 2;
  shift_y := h_to mod s / 2;
  let rec pick_random_space () =
    let x, y = (int 30, int 30) in
    match data.(x).(y) with
    | Wall -> pick_random_space ()
    | Floor _ ->
        if min x y <= 5 or max x y >= s - 6 then pick_random_space () else (x, y)
  in
  let start = () |> pick_random_space |> to_sdl_area in
  { data; start; size = (s, s) }

(* mutate map data to include a random item at a Floor cell *)
let add_item map = ()

let color_of_rgb c a =
  match c with
  | r, g, b -> (r, g, b, a)

let draw_circle sdl_area c r loc =
  Bogue.Sdl_area.fill_circle sdl_area ~color:c ~radius:r loc

let draw_rect sdl_area c w h loc =
  Bogue.Sdl_area.fill_rectangle sdl_area ~color:c ~w ~h loc

let draw_wall map sdl_area (x, y) =
  let module D = Bogue.Draw in
  let w, h = (!scale_x, !scale_y) in
  let grey = color_of_rgb D.dark_grey 255 in
  let w_2, h_2 = (w / 2, h / 2) in
  let x_0, y_0 = ((x * w) + !shift_x, (y * h) + !shift_y) in
  draw_circle sdl_area grey
    ((w_2 + h_2) / 4) (* take average *)
    (x_0 + w_2, y_0 + h_2);
  let { data; size; _ } = map in
  let x_max, y_max = size in
  let top, right, bottom, left =
    ( y != 0 && data.(x).(y - 1) = Wall,
      x < x_max - 1 && data.(x + 1).(y) = Wall,
      y < y_max - 1 && data.(x).(y + 1) = Wall,
      x != 0 && data.(x - 1).(y) = Wall )
  in
  let w_4, h_4 = (w / 4, h / 4) in
  let w_2, h_2 = (w_2 + 1, h_2 + 1) in
  let draw_grey_rect = draw_rect sdl_area grey in
  if top then draw_grey_rect w_2 h_2 (x_0 + w_4, y_0);
  if right then draw_grey_rect w_2 h_2 (x_0 + w_2, y_0 + h_4);
  if bottom then draw_grey_rect w_2 h_2 (x_0 + w_4, y_0 + h_2);
  if left then draw_grey_rect w_2 h_2 (x_0, y_0 + h_4)

let draw_floor c sdl_area (x, y) =
  let module D = Bogue.Draw in
  let w, h = (!scale_x, !scale_y) in
  (* draw background *)
  draw_rect sdl_area c w h ((x * w) + !shift_x, (y * h) + !shift_y);
  (* draw boundary *)
  draw_rect sdl_area
    (color_of_rgb D.dark_grey 100)
    (w + 2) (h + 2)
    ((x * w) + !shift_x - 1, (y * h) + !shift_y - 1)

let draw_map sdl_area map =
  let w_from, h_from = map.size in
  let c = (int 255, int 255, int 255, 80) in
  for x = 0 to w_from - 1 do
    for y = 0 to h_from - 1 do
      let p = (x, y) in
      match map.data.(x).(y) with
      | Wall ->
          (* swap drawing order to disable color layering *)
          draw_wall map sdl_area p;
          draw_floor c sdl_area p
      | Floor _ -> draw_floor c sdl_area p
    done
  done
