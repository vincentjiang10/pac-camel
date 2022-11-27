open State

type item =
  | BigCoin
  | SmallCoin
  | Coins
  | Speed
  | Traj
  | Sand
  | Phase
  | Cactus
  | Tele
  | Dim
  | Life
  | Time

type t = {
  width : int;
  height : int;
  probabilty : float;
  start_time : int;
  duration : int;
  src : string;
  effect : unit -> unit;
  animate : t ref -> unit;
  item_type : item;
}

let size t = (t.width, t.height)
let startTime t = t.start_time
let duration t = t.duration
let probability t = t.probabilty
let src t = t.src

(* will depend on State.state_time *)
let animate t = !t.animate t
let effect t = t.effect ()
let item_type t = t.item_type
let itemWidth = ref 0
let itemHeight = ref 0
let path = "assets/images/items/"

let commonItem () =
  {
    width = !itemWidth;
    height = !itemHeight;
    probabilty = 0.0005;
    start_time = !state_time;
    (* 10 seconds *)
    duration = 1000;
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
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
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
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
    (* 1 minute *)
    duration = !state_end_time;
    src = path ^ "coin.png";
    effect = (fun () -> incr state_score);
    animate = (fun item_ref -> ());
    item_type = SmallCoin;
  }

(* Power-up creation *)

(* double coin values *)
let coinsItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Coins;
  }

(* doubles camel speed *)
let speedItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Speed;
  }

(* show human trajectory *)
let trajectoryItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Traj;
  }

(* stuns players *)
let sandItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Sand;
  }

(* allow phasing through walls *)
let phaseItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Phase;
  }

(* scares away humans *)
let cactusItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Cactus;
  }

(* teleports player to another teleportItem *)
(* TODO: figure out how to handle multiple of these items. Either allow camel to
   teleport to another random teleport item, or stricly allow only two teleport
   items to exist at a time *)
let teleportItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Tele;
  }

(* dims the lighting of the map to only the camel (like a spotlight) *)
let dimItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Dim;
  }

(* gives an additional life to the camel *)
let lifeItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> ());
    animate = (fun item_ref -> ());
    item_type = Life;
  }

(* gives additional time (10 seconds) until game round ends*)
let timeItem () =
  let commonItem = commonItem () in
  {
    commonItem with
    src = "";
    effect = (fun () -> state_time := !state_time + 1000);
    animate = (fun item_ref -> ());
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
    trajectoryItem;
    sandItem;
    phaseItem;
    cactusItem;
    teleportItem;
    dimItem;
    lifeItem;
    timeItem;
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
