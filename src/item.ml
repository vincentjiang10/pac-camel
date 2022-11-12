type t = {
  probabilty : float;
  period : int;
  src : string;
  effect : unit;
  animate : unit;
}

let period t = t.period
let probability t = t.probabilty
let src t = t.src
let animate t = t.animate
let effect t = t.effect

(* TODO: add small and big coins *)

(* Power-up creation *)

let coinsItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* TODO: fix problem with camel crossing at intersections *)
let speedItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let trajectoryItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let sandItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let phaseItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let cactusItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let teleportItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

let dimItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }
