type item =
  | Coins
  | Speed
  | Traj
  | Sand
  | Phase
  | Cactus
  | Tele
  | Dim
  | Life

type t = {
  probabilty : float;
  period : int;
  src : string;
  effect : unit;
  animate : unit;
  itemType : item;
}

let period t = t.period
let probability t = t.probabilty
let src t = t.src
let animate t = t.animate
let effect t = t.effect
let itemType t = t.itemType

(* TODO: add small and big coins *)

(* Power-up creation *)

let coinsItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Coins;
  }

(* TODO: fix problem with camel crossing at intersections *)
let speedItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Speed;
  }

let trajectoryItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Traj;
  }

let sandItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Sand;
  }

let phaseItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Phase;
  }

let cactusItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Cactus;
  }

let teleportItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Tele;
  }

let dimItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Dim;
  }

let lifeItem () =
  {
    probabilty = 0.01;
    period = 10;
    src = "";
    effect = ();
    animate = ();
    itemType = Life;
  }
