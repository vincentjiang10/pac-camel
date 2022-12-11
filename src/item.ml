open State

type item =
  | BigCoin
  | SmallCoin
  | Coins
  | Speed
  | Sand
  | Phase
  | Cactus
  | Life
  | Time
  | Invincible

type t = {
  width : int;
  height : int;
  probabilty : float;
  start_time : int;
  duration : int;
  effect_duration : float;
  src : string;
  effect_start : unit -> unit;
  effect_end : unit -> unit;
  animate : t ref -> unit;
  shift : int * int;
  flip : Tsdl.Sdl.flip;
  item_type : item;
}

let size t = (t.width, t.height)
let startTime t = t.start_time
let duration t = t.duration
let probability t = t.probabilty
let src t = t.src

(* will depend on State.state_time *)
let animate t = !t.animate t
let shift t = t.shift

let make_effect delay effect () =
  Thread.create
    (fun () ->
      Thread.delay delay;
      effect ())
    ()
  |> ignore

let effect t =
  t.effect_start ();
  make_effect t.effect_duration t.effect_end ()

let flip t = t.flip
let item_type t = t.item_type
let itemWidth = ref 0
let itemHeight = ref 0
let path = "assets/images/items/"

(*========================== Transformation effects ==========================*)
let change_flip item_ref =
  let item = !item_ref in
  item_ref :=
    {
      item with
      flip = Tsdl.Sdl.Flip.(if item.flip = none then horizontal else none);
    }

let cos_time rate c = cos (c +. ((!state_time |> float_of_int) *. rate))

let rotate_y max_size item_ref =
  (* TODO: flip source image *)
  let item = !item_ref in
  let width =
    let scale = cos_time 0.02 (item.start_time |> float_of_int) in
    (max_size |> float_of_int) *. scale |> int_of_float |> abs
  in
  (* x-coordinate of item location is moved half of size dilation *)
  let shift = ((max_size - width) / 2, 0) in
  item_ref := { item with width; shift };
  (* change direction upon reaching trough in width *)
  if width = 0 then change_flip item_ref

(*============================================================================*)

let commonItem () =
  {
    width = !itemWidth;
    height = !itemHeight;
    probabilty = 0.001;
    start_time = !state_time;
    (* 20 seconds *)
    duration = 2000;
    effect_duration = 10.;
    src = "";
    effect_start = (fun () -> ());
    effect_end = (fun () -> ());
    (* default animation *)
    animate = rotate_y !itemWidth;
    shift = (0, 0);
    flip = Tsdl.Sdl.Flip.none;
    item_type = Life;
  }

(* TODO: implement effect to increment state_num_coins for both small and big
   coins *)
(* big coin *)
let bigCoin () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = path ^ "coin.png";
    effect_start =
      (* amount state_score is incremented is dependent on camel state *)
      (fun () ->
        let incr = if state_camel.doubleCoin then 50 else 25 in
        state_score := !state_score + incr);
    item_type = BigCoin;
  }

(* small coin *)
let smallCoin () =
  let commonItem = commonItem () in
  {
    commonItem with
    width = !itemWidth / 2;
    height = !itemHeight / 2;
    probabilty = 0.5;
    animate = rotate_y (!itemWidth / 2);
    (* 1 minute *)
    duration = !state_end_time;
    src = path ^ "coin.png";
    effect_start =
      (fun () ->
        let incr = if state_camel.doubleCoin then 2 else 1 in
        state_score := !state_score + incr);
    item_type = SmallCoin;
  }

(* Power-up creation *)

(* double coin values *)
let coinsItem () =
  let commonItem = commonItem () in
  let setDoubleCoin b () = state_camel.doubleCoin <- b in
  {
    commonItem with
    src = path ^ "coin_pile.png";
    effect_start = setDoubleCoin true;
    effect_end = setDoubleCoin false;
    item_type = Coins;
  }

(* doubles camel speed *)
let speedItem () =
  let commonItem = commonItem () in
  let setDoubleSpeed b () = state_camel.doubleSpeed <- b in
  {
    commonItem with
    src = path ^ "speed.png";
    effect_start = setDoubleSpeed true;
    effect_end = setDoubleSpeed false;
    item_type = Speed;
  }

(* stuns players *)
let sandItem () =
  let commonItem = commonItem () in
  let setHalfSpeed b () = state_human.halfSpeed <- b in
  {
    commonItem with
    src = path ^ "sand.jpg";
    effect_start = setHalfSpeed true;
    effect_end = setHalfSpeed false;
    item_type = Sand;
  }

(* allow phasing through walls *)
let phaseItem () =
  let commonItem = commonItem () in
  let setIgnoreWalls b () =
    state_camel.ignoreWalls <- b;
    if not b then state_camel.doubleSpeed <- false
  in
  {
    commonItem with
    src = path ^ "potion.png";
    effect_start = setIgnoreWalls true;
    effect_end = setIgnoreWalls false;
    item_type = Phase;
  }

(* scares away humans *)
let cactusItem () =
  let commonItem = commonItem () in
  let setScared b () = state_human.scared <- b in
  {
    commonItem with
    src = path ^ "cactus.png";
    effect_start = setScared true;
    (* TODO: set scared and double speed to false *)
    effect_end = setScared false;
    item_type = Cactus;
  }

(* gives invincibility state *)
let invincibleItem () =
  let commonItem = commonItem () in
  let setInvincible b () = state_camel.invincible <- b in
  {
    commonItem with
    src = path ^ "shield.png";
    effect_start = setInvincible true;
    effect_end = setInvincible false;
    item_type = Invincible;
  }

(* gives an additional life to the camel *)
let lifeItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = path ^ "heart.png";
    effect_start = (fun () -> state_lives := min 3 (!state_lives + 1));
    item_type = Life;
  }

(* gives additional time (10 seconds) until game round ends*)
let timeItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = path ^ "time.png";
    effect_start = (fun () -> state_end_time := !state_end_time + 1000);
    item_type = Time;
  }

let init_items (w, h) =
  itemWidth := w;
  itemHeight := h

let init_item_list : (unit -> t) list =
  [
    bigCoin;
    smallCoin;
    coinsItem;
    speedItem;
    sandItem;
    phaseItem;
    cactusItem;
    lifeItem;
    timeItem;
    invincibleItem;
  ]

(* finds the increasing cumulative sums of the probabilities of the items in
   [itemList] *)
let cumul_probs item_lst =
  let rec get_cumul_probs (acc : float list) = function
    | [] -> acc
    | h :: t ->
        let prob = h.probabilty in
        let prev_prob =
          match acc with
          | [] -> 0.
          | h :: _ -> h
        in
        get_cumul_probs ((prob +. prev_prob) :: acc) t
  in
  1. :: get_cumul_probs [] item_lst |> List.rev

(* has length that is one more than init_item_list *)
let cumul_probs =
  List.map (fun init_items -> init_items ()) init_item_list |> cumul_probs

let gen_rand_item () =
  (* binary seach to find closest index in [cumul_probs] whose element is the
     smallest element in [cumul_probs] greater than [targ] *)
  let rec bin_search targ (lower_in, upper_in) =
    if lower_in > upper_in then failwith "binary search: impossible";
    let cs = cumul_probs in
    let mid_in = (lower_in + upper_in) / 2 in
    if List.nth cs mid_in >= targ then
      if mid_in = 0 || List.nth cs (mid_in - 1) < targ then mid_in
      else (* [mid_in] too large *) bin_search targ (lower_in, mid_in - 1)
    else bin_search targ (mid_in + 1, upper_in)
  in
  let ind = bin_search (Random.float 1.) (0, List.length init_item_list) in
  match List.nth_opt init_item_list ind with
  | None -> None
  | Some init_item -> Some (ref (init_item ()))
