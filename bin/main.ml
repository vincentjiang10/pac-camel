open Game
open Pacmap

let play = false

(* will include draw_map on generated map *)
let reset (seed : int) =
  let map = gen_map seed in
  (* draw map*)
  map

let playOrPause () = ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget events)*)
let startGUI () = ()

let main () =
  let _ = startGUI () in
  let _ = reset 1 in
  ()

(* Execute game *)
let () = main ()