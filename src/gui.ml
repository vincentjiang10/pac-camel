(* Contains GUI logic *)
open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger

(* SETUP *)
(* WIDGETS*)
let start_title_w =
  W.label ~size:32
    ~fg:Draw.(opaque (find_color "firebrick"))
    "The Pac Camel Game"

let start_button_w = W.button "Start the Game"

(*LAYOUT*)
let start_title_l = Layout.resident start_title_w ~y:2
let start_button_l = Layout.resident ~x:100 ~y:25 ~h:10 start_button_w

let camel_image =
  let src = W.image ~w:40 ~h:40 "data/images/camel-cartoon.png" in
  L.flat ~name:"image" [ L.resident src ]

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget events)*)
let greeting =
  (* set what to be drawn *)
  let layout = L.superpose [ start_title_l; start_button_l ] in
  (* set default window size *)
  let _ = Timeout.add 1 (fun () -> L.set_size layout (800, 600)) in

  (*action to be connected to start button*)
  let start_action _ _ _ =
    print_endline "start";
    L.set_rooms layout [ camel_image ]
    (*this line replace current layout with an empty list, add widgets (to be
      drawn after start) in this list e.g. map, camel, human etc. *)
  in
  (* connect action to button. Triggered when button is pushed*)
  let c = W.connect start_button_w start_button_w start_action T.buttons_down in

  (* set up board *)
  let board = of_layout ~connections:[ c ] layout in
  run board