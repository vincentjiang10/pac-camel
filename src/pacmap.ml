open State
open Item
open Random

(* NOTE: the drawing of items will not be on sdl_area but on the window
   renderer *)
(* A function written will be called that takes in map t and renders the items
   with their respective animations *)
type space =
  | Empty
  | Mass of Item.t ref

type cell =
  | Wall
  | Floor of space

type t = {
  data : cell array array;
  item_list : ((int * int) * Item.t ref) list;
  start : int * int;
  size : int * int;
}

let scale_x = ref 0
let scale_y = ref 0
let shift_x = ref 0
let shift_y = ref 0

let to_sdl_area (x, y) =
  ( (x * !scale_x) + !shift_x - (!scale_x / 4),
    (y * !scale_y) + !shift_y - (!scale_y / 4) )

let to_canvas (x, y) =
  ( (x - !shift_x + (!scale_x / 4)) / !scale_x,
    (y - !shift_y + (!scale_y / 4)) / !scale_y )

let apply f (x, y) = (f x, f y)

let string_of_point (x, y) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let find_move map p_from dir =
  let size = fst map.size in
  let bound v = if v < 0 then v + size else if v >= size then v - size else v in
  let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1) in
  let p_to = dir |> apply (( * ) !scale_x) |> add p_from in
  let x, y = p_to |> to_canvas |> apply bound in
  match map.data.(x).(y) with
  | Wall ->
      if state_camel.ignoreWalls then (to_sdl_area (x, y), Empty)
      else (p_from, Empty)
  | Floor space -> (to_sdl_area (x, y), space)

let valid_xy s (x, y) = min x y >= 0 && max x y < s

let unit_size () =
  let float_scale n = n |> Int.to_float |> Float.mul 1.5 |> Float.to_int in
  (!scale_x, !scale_y) |> apply float_scale

let camel_ctx t = (t.start, unit_size (), 2)

let human_init_pos t ind =
  let w, h = t.size in
  (* set initial position of humans to the center of map *)
  ((w / 2) - 2 + ind, h / 2) |> to_sdl_area

let human_ctx t ind = (human_init_pos t ind, unit_size (), 1)

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

(* Boundary points: points that are outside the map boundary *)
let boundary_points = ref PointSet.empty
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

type contraint_info = {
  mutable x_range : int list;
  mutable y_range : int list;
}

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
    let contraints = { x_range = []; y_range = [] } in
    (* Requires: dir must be a unit vector with magnitude of x or y being 1 *)
    let contraint_maker dir (x, y) =
      match dir with
      | 0, _ -> contraints.y_range <- y :: contraints.y_range
      | _, 0 -> contraints.x_range <- x :: contraints.x_range
      | _ -> failwith "Impossible: precondition violated"
    in
    (* first contraint *)
    contraint_maker (dir_x, dir_y) (x, y);
    let p = walls (dir_x, dir_y) (x, y) l1 in
    (* second contraint *)
    contraint_maker (dir_y, -dir_x) p;
    let p = walls (dir_y, -dir_x) p l2 in
    (* third contraint *)
    contraint_maker (-dir_x, -dir_y) p;
    let p_x, p_y = walls (-dir_x, -dir_y) p l1 in
    (* fourth contraint *)
    contraint_maker (-dir_y, dir_x) (p_x, p_y);

    (* add points satifying contraints to boundary points *)
    let xs = List.sort compare contraints.x_range in
    let ys = List.sort compare contraints.y_range in
    for x = List.nth xs 0 to List.nth xs 1 do
      for y = List.nth ys 0 to List.nth ys 1 do
        add (x, y) boundary_points
      done
    done;

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
        if data.(x).(y) = Wall then add (x, y) wall_set;
        if PointSet.mem (x, s - 1 - y) !boundary_points then
          add (x, y) boundary_points
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
   walls. *)
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
      let len = 2 + (s / 20) + int 4 in
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
  gen_pieces ()

(* Copies the right half of data over the y-axis to the left *)
let mirror_left_y data s =
  let mid = s / 2 in
  for x = 0 to mid - 1 do
    for y = 0 to s - 1 do
      data.(x).(y) <- data.(s - 1 - x).(y);
      if PointSet.mem (s - 1 - x, y) !boundary_points then
        add (x, y) boundary_points
    done
  done

(* Returns a list containing locations of initial items and the reference to the
   items themselves *)
let populate_items data s =
  let item_list_ref = ref [] in
  (* adding initial items to map *)
  for x = 0 to s - 1 do
    for y = 0 to s - 1 do
      (* should not add item to boundary points *)
      if PointSet.mem (x, y) !boundary_points then ()
      else if data.(x).(y) = Floor Empty then
        data.(x).(y) <-
          Floor
            (let item_ref = gen_rand_item () in
             match item_ref with
             | None -> Empty
             | Some item_ref ->
                 if Item.item_type !item_ref = SmallCoin then incr state_num_coins;
                 let sdl_loc = to_sdl_area (x, y) in
                 item_list_ref := (sdl_loc, item_ref) :: !item_list_ref;
                 Mass item_ref)
    done
  done;
  !item_list_ref

(*================================ Pathfinding ===============================*)

module PointMap = Map.Make (Point)

(* [!paths.(src_in).(dest_in)] is the unit direction (as a Point) to progress in
   traversing from point [src = (x_s, y_s)] to point [dst = (x_d, y_d)], where
   [src_in = x_s * s + y_s], [dst_in = x_d * s + y_d], and [s = Array.length
   !paths]. *)
let paths = ref [||]

let rec pick_random_space data =
  let len = Array.length data in
  let x, y = (int 30, int 30) in
  match data.(x).(y) with
  | Wall -> pick_random_space data
  | Floor _ ->
      if
        min x y <= 5
        || max x y >= len - 6
        || x > (len / 2) - 5
           && x < (len / 2) + 5
           && y > (len / 2) - 5
           && y < (len / 2) + 5
      then pick_random_space data
      else (x, y)

(* Precomputing path lengths on gen_map using BFS *)
let precompute_paths data =
  let len = Array.length data in
  let to_ind (x, y) = (x * len) + y in
  (* [bfs_trav] is the bfs traversal of [data] starting at [src]. Requires:
     [src] is not a [Wall] *)
  let bfs_trav src =
    let src_ind = src |> to_ind in
    (* for any point/node [dst], it is visited by the traversal with the
       original source point at [src] if !paths.(src |> to_ind).(dst |> to_ind)
       is not (0,0) *)
    let check_node dir dst queue =
      let dst_ind = dst |> to_ind in
      let x, y = dst in
      if
        (dst_ind >= 0 && dst_ind < Array.length !paths)
        && min x y >= 0
        && max x y < Array.length data
        && data.(fst dst).(snd dst) <> Wall
        && !paths.(src_ind).(dst_ind) = (0, 0)
      then begin
        (* add to queue *)
        Queue.add dst queue;
        (* modify [!paths] *)
        !paths.(src_ind).(dst_ind) <- dir
      end
    in
    let queue = Queue.create () in
    Queue.add src queue;
    (* substitute for [true] to mark [src] visited *)
    !paths.(src_ind).(src_ind) <- (0, 0);
    while Queue.length queue <> 0 do
      let x, y = Queue.pop queue in
      (* finding adjacent nodes (that are spaces) *)
      check_node (1, 0) (x - 1, y) queue;
      check_node (-1, 0) (x + 1, y) queue;
      check_node (0, 1) (x, y - 1) queue;
      check_node (0, -1) (x, y + 1) queue
    done
  in

  paths := Array.make_matrix (len * len) (len * len) (0, 0);

  for x = 0 to len - 1 do
    for y = 0 to len - 1 do
      (* if a src point is not a [Wall], then perform bfs traversal on that
         point *)
      if data.(x).(y) <> Wall then bfs_trav (x, y)
    done
  done

let get_path_dir map src dst =
  (* approximate position of dst *)
  let len = fst map.size in
  let pos_invert x = if float 1. > 0.5 then x else ~-x in
  let bound x = if x >= len then len - 1 else if x < 0 then 0 else x in
  (* find a nearby loc that is a manhattan distance of [man_dist] to [(x, y)] *)
  let rec spread man_dist (x, y) =
    let x_shift = if man_dist = 0 then 0 else int man_dist in
    let y_shift = man_dist - x_shift in
    let x_shift, y_shift = (x_shift, y_shift) |> apply pos_invert in
    (x + x_shift, y + y_shift) |> apply bound
  in
  let man_dist (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0) in
  let to_ind (x, y) = (x * Array.length map.data) + y in
  let src, dst = apply to_canvas (src, dst) in
  (* call on [spread] has parameter [man_dst] that is less than the manhattan
     distance between [src] and [dst] (to approach closer to [dst] from
     [src]) *)
  let man_dist = if state_human.scared then 0 else man_dist src dst / 10 in
  !paths.(dst |> spread man_dist |> to_ind).(src |> to_ind)

(*============================================================================*)

(* Creates a pacmap with random odd size *)
let gen_map seed sdl_area =
  init seed;
  let s = (int 20 * 2) + 30 in
  (* reset end time to be dependent on n^2, where n is the size of the map *)
  state_end_time := s * s * 10;
  state_num_coins := 0;
  let data = Array.make_matrix s s (Floor Empty) in
  (* emptying sets *)
  empty wall_set;
  empty wall_boundary;
  empty boundary_points;
  (* populating map with walls *)
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
  unit_size () |> init_items;
  let start = data |> pick_random_space |> to_sdl_area in
  let item_list = populate_items data s in
  { data; start; size = (s, s); item_list }

(*================================== Item Logic ==============================*)

(* mutate map data to include a random item at a Floor cell and add item and its
   location to item_list; calls on gen_rand_item *)
let add_item map_ref =
  let map = !map_ref in
  let is_item map (x, y) =
    if
      x < 0 || y < 0
      || x >= fst map.size
      || y >= snd map.size
    then false
    else
      match map.data.(x).(y) with
      | Floor (Mass _) -> true
      | _ -> false
  in
  let check_neighbor map (x, y) =
    let neighbor_lst =
      [
        (x, y);
        (x + 1, y);
        (x + 1, y + 1);
        (x, y + 1);
        (x + 1, y - 1);
        (x - 1, y);
        (x - 1, y + 1);
        (x - 1, y - 1);
        (x, y - 1);
      ]
    in
    List.fold_left (fun acc loc -> acc || is_item map loc) false neighbor_lst
  in
  let rec return_floor_loc map n =
    if n = 0 then None
    else
      let w, h = map.size in
      let data = map.data in
      let x, y = (int w, int h) in
      match data.(x).(y) with
      | Floor _ ->
          (* check neighboring floors do not contain a small coin *)
          if 
            (* should not be in boundary points *)
            PointSet.mem (x, y) !boundary_points || check_neighbor map (x, y) then return_floor_loc map (n - 1)
          else Some (x, y)
      | Wall -> return_floor_loc map (n - 1)
  in
  (* try for 10 iterations only *)
  match return_floor_loc map 10 with
  | Some (x, y) ->
      let item_list_ref = ref map.item_list in
      let item_ref_opt = gen_rand_item () in
      map.data.(x).(y) <-
        Floor
          begin
            match item_ref_opt with
            | None -> Empty
            | Some item_ref -> begin
                match item_type !item_ref with
                | SmallCoin -> Empty (* do not add item if it's a small coin *)
                | _ ->
                    let sdl_loc = to_sdl_area (x, y) in
                    item_list_ref := (sdl_loc, item_ref) :: !item_list_ref;
                    Mass item_ref
              end
          end;
      (* mutate contents of map_ref *)
      map_ref := { !map_ref with data = map.data; item_list = !item_list_ref }
  | None -> ()

let remove_item map_ref loc =
  let x, y = to_canvas loc in
  let map = !map_ref in
  (* remove item *)
  map.data.(x).(y) <- Floor Empty;
  let item_list = List.remove_assoc loc map.item_list in
  map_ref := { map with item_list }

let check_item_expiration map_ref =
  let map = !map_ref in
  (* iterates over item list *)
  let item_list =
    List.filter
      (fun (sdl_loc, item_ref) ->
        let item = !item_ref in
        let time_elapsed = !state_time - startTime item in
        let expired = time_elapsed > duration item in
        if expired then begin
          let x, y = to_canvas sdl_loc in
          map.data.(x).(y) <- Floor Empty;
          false
        end
        else true)
      map.item_list
  in
  map_ref := { map with item_list }

let get_items map = map.item_list
let animate_items item_list = List.iter Item.animate item_list

(*================================== Drawing =================================*)
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
  if top then begin
    draw_grey_rect w_2 h_2 (x_0 + w_4, y_0);
    draw_grey_rect w_2 h_2 (x_0 + w_4, y_0)
  end;
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
      | Floor _ ->
          (* TODO: add additional texture for boundary points *)
          (* if PointSet.mem (x, y) !boundary_points then draw_wall map sdl_area p; *)
          draw_floor c sdl_area p;
    done
  done
