open Bogue
open Main
open Pacmap
open Movable
open Camel
open Human
open Utils
open State
open Random
module W = Widget
module L = Layout
module T = Trigger

(* SETUP *)

let width = 800
let height = 800

(* WIDGETS*)
let start_title_w =
  W.label ~size:32
    ~fg:Draw.(opaque (find_color "firebrick"))
    "The Pac-Camel Game"

let start_button_w = W.button "Press [s] to Start the Game"

(*LAYOUT*)

(* TODO @GUI (optional): animate in the title + add hovering to buttons upon
   user mouse hover event *)
(* TODO (add keyboard options for quit (Q), play/pause (spacebar or P), and H (H
   or ?); these would trigger events that the buttons that they correspond to
   trigger *)
let start_title_l = L.resident start_title_w ~y:2
let start_button_l = L.resident ~x:30 ~y:35 ~w:250 ~h:2 start_button_w

(* TODO @GUI (post-MS2): Implement more widgets 
 *  - score displayer
 *  - time displayer (perhaps we can set a time limit for the player)
 *  - a play/pause button
 *  - a reset button (has the same action as start button, just with the text changed) 
 *  - a text field to generate a map with a specific seed -> a button to apply (or just reset) 
 *  - a help button to open up a popup that shows the user the different features that are implemented
 *  - (optional) an algorithm selection drop down menu that selects what algorithm that the humans follow 
 *  - (optional) a speed and number of humans setting (set a minimum and maximum)
 *)

let canvas = W.sdl_area ~w:800 ~h:800 ()
let canvas_l = L.resident ~x:0 ~y:0 canvas
let sdl_area = W.get_sdl_area canvas

(* reference to map *)
let map_ref = ref (gen_map (int 500) sdl_area)

(* reference to camel *)
let camel_ref = ref (Camel.init !map_ref "assets/images/camel.png")

(* references to humans *)

let rec human_inits acc n =
  if n = 0 then acc
  else
    human_inits
      (ref (Human.init !map_ref "assets/images/human.png") :: acc)
      (n - 1)

let human_ref_lst = ref (human_inits [] 4)

let reset_map (seed : int) =
  (* reset canvas *)
  Sdl_area.clear sdl_area;
  map_ref := gen_map seed sdl_area;
  camel_ref := Camel.init !map_ref "assets/images/camel.png";
  human_ref_lst := human_inits [] 4;
  draw_map sdl_area !map_ref

let time_ref = ref 0
let reset_time () = time_ref := Time.now ()

(* sets up the game *)
let reset_game seed =
  reset_map seed;
  reset_time ();
  state_time := 0;
  state_score := 0

let bg = (255, 255, 255, 255)

let make_greeting_board =
  let greeting_layout = L.superpose [ start_title_l; start_button_l ] in
  L.set_width greeting_layout 800;
  L.set_height greeting_layout 800;

  of_layout greeting_layout

let make_game_board =
  (* set what to be drawn *)
  (* TODO @GUI: clicking on widgets do not work: try to fix *)
  let layout = L.superpose [ canvas_l ] in
  L.set_width layout 385;
  L.set_height layout 385;
  of_layout layout

let board = ref make_greeting_board
let state = init_state ()

let main () =
  let open Tsdl in
  Sys.catch_break true;
  go (Sdl.init Sdl.Init.video);
  let win =
    go
      (Sdl.create_window ~w:800 ~h:800 "Pac-Camel Game"
         Sdl.Window.(shown + popup_menu))
  in

  let renderer = go (Sdl.create_renderer win) in

  (* TODO @Yaqi: camel texture has a black background while human does not; try
     to fix that bug *)
  let camel_texture =
    let camel_surface = Tsdl_image.Image.load (Camel.src !camel_ref) in
    let t = Sdl.create_texture_from_surface renderer (go camel_surface) in
    go t
  in

  let human_texture =
    let human_surface =
      Tsdl_image.Image.load (Human.src !(List.hd !human_ref_lst))
    in
    let t = Sdl.create_texture_from_surface renderer (go human_surface) in
    go t
  in
  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);

  go (Sdl.render_clear renderer);

  make_sdl_windows ~windows:[ win ] !board;
  let start_fps, fps = Time.adaptive_fps 300 in

  let camel_dir_ref = ref (0, 0) in

  (* TODO: add trailing effect behind camel (can be an effect )*)
  (* TODO: fix error with delayed arrow movement *)
  let rec mainloop e =
    let camel = !camel_ref in
    let camel_dir = !camel_dir_ref in
    let humans = !human_ref_lst in
    let map = !map_ref in
    let move_check dir = camel_dir_ref := dir in

    (* if paused, then disable the below internally, not the whole match body *)
    (if Sdl.poll_event (Some e) then
     match current_state state with
     | Active -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
             move_check (0, -1)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
             move_check (0, 1)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
             move_check (1, 0)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
             move_check (-1, 0)
         | `Key_down
           when List.mem
                  Sdl.Event.(get e keyboard_keycode)
                  [ Sdl.K.r; Sdl.K.space ] ->
             print_endline "restart";
             camel_dir_ref := (0, 0);
             reset_game (int 10000)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.p ->
             change_state state Pause
         | _ -> ())
     | Inactive -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.s ->
             print_endline "game start";
             camel_dir_ref := (0, 0);
             let change_board () =
               board := make_game_board;
               change_state state Active
             in
             (* let th : Thread.t = Thread.create Sync.push change_board in *)
             let th : Thread.t = Thread.create change_board () in
             Thread.join th;
             let make_window () = make_sdl_windows ~windows:[ win ] !board in
             let th2 : Thread.t = Thread.create make_window () in
             Thread.join th2;
             reset_game (int 10000)
         | _ -> ())
     | Pause -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.p ->
             change_state state Active
         | _ -> ())
     | _ -> ());

    Draw.set_color renderer bg;

    (* update time counter *)
    incr state_time;

    go (Sdl.render_clear renderer);
    let x_c, y_c = Camel.pos camel in
    let w, h = Camel.size camel in
    (* replace render_fill_rect with rendering an image of a camel *)
    refresh_custom_windows !board;

    if
      not (one_step true (start_fps, fps) !board)
      (* one_step returns true if fps was executed *)
    then fps ()
    else fps ();

    (* checks item expiration *)
    check_item_expiration map_ref;

    (* item rendering *)
    (* TODO @Yaqi: replace rectangles with images *)
    let item_list = get_items map in
    let new_rect (w, h) (x, y) = Sdl.Rect.create ~x ~y ~w ~h in
    Draw.set_color renderer (100, 200, 200, 255);

    if current_state state != Inactive then (
      List.iter
        (fun ((x, y), item_ref) ->
          let size = Item.size !item_ref in
          let loc =
            match Item.item_type !item_ref with
            | SmallCoin -> (x + (fst size / 2), y + (snd size / 2))
            | _ -> (x, y)
          in
          go (Sdl.render_fill_rect renderer (Some (new_rect size loc)))
          |> ignore)
        item_list;

      (* Camel and human rendering happens after fps so that they are on top of
         the map. *)

      (* camel rendering logic *)
      let render_camel () =
        let x, y = Camel.pos camel in
        go
          (Sdl.render_copy
             ?dst:(Some (Sdl.Rect.create ~x ~y ~w ~h))
             renderer camel_texture)
      in
      render_camel ();

      let camel_spd = Camel.speed camel in
      let camel_period = 30 / camel_spd in
      if camel_spd <> 0 && !state_time mod camel_period = 0 then
        Camel.move camel_ref map_ref camel_dir (fun () -> ());

      (* human rendering logic *)
      List.iteri
        (fun i human ->
          (* abstract into a function, then use this in tween *)
          let x_h, y_h = Human.pos !human in
          let w, h = Human.size !human in
          let render_human () =
            let x, y = Human.pos !human in
            go
              (Sdl.render_copy
                 ?dst:(Some (Sdl.Rect.create ~x ~y ~w ~h))
                 renderer human_texture)
          in
          (* TODO: increase inversely proportional to number of coins (retrieved
             from State) *)
          let human_spd = Human.speed !human in
          let human_period = 30 / human_spd in
          (if
           human_spd <> 0
           && !state_time mod human_period = 0
           && Time.now () - !time_ref > (i * 10000) + 1000
           && (x_h <> x_c || y_h <> y_c)
          then
           let dir = get_path_dir map (x_h, y_h) (x_c, y_c) in
           Human.move human map_ref dir render_human);
          render_human ())
        humans);

    if current_state state = Pause then (
      Draw.set_color renderer (255, 255, 255, 100);
      go
        (Sdl.render_fill_rect renderer
           (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:800 ~h:800))));
    Sdl.render_present renderer;
    mainloop e
  in

  let e = Sdl.Event.create () in
  start_fps ();
  let () = try mainloop e with _ -> exit 0 in
  Sdl.destroy_window win;
  Draw.quit ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget
   events) *)
let greeting =
  reset_game (int 5000);
  main ()
