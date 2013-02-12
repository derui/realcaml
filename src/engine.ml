type t = {
  time_step:float;
  contact_bias:float;
  contact_stop:float;
  iteration:int;
  max_bodies:int;
  max_pairs:int;

  (* manage rigid body informations and collisions for them *)
  sweep_prune : SweepPrune.t;

  pair_count:int array;
  mutable pair_swap:int;
  mutable pair: Pair.t array array;
}

let make ?(time_step=0.016) ?(contact_bias=0.1) ?(contact_stop=0.001)
  ?(iteration=10) ?(max_bodies=500) ?(max_pairs=5000) () =
  {time_step; contact_bias; contact_stop; iteration; max_bodies; max_pairs;
   sweep_prune = SweepPrune.make max_bodies;
   pair_swap = 0;
   pair_count = [|0; 0|];
   pair = Array.make_matrix 2 max_pairs Pair.empty;
  }

(* rigid body同士の衝突判定を行う。 *)
let intersect_bodies engine =
  let sp = engine.sweep_prune in
  let max_count = SweepPrune.get_max_count sp in
  let swap = engine.pair_swap in
  let pair_count = ref 0 in
  let pairs = engine.pair.(swap) in
  let rec intersect_loop sp pairs base_count other_count =
    if base_count >= max_count then pairs
    else if other_count >= max_count then
      intersect_loop sp pairs (succ base_count) (other_count + 2)
    else
      (* TODO: それぞれの衝突判定を行う。 *)
      match SweepPrune.intersect sp base_count other_count with
      | None -> intersect_loop sp pairs base_count (succ other_count)
      | Some (body1, body2) ->
        (* There are not enough contact point to fill contact module now,
           so contact information is not set.
        *)
        pairs.(!pair_count) <- Pair.make_by_index ~pt:Pair.New ~indexA:(Int32.of_int base_count)
          ~indexB:(Int32.of_int other_count) ();
        pair_count := succ !pair_count;
        intersect_loop sp pairs base_count (succ other_count) in
  let pairs = intersect_loop sp pairs 0 1 in
  (pairs, swap, !pair_count)

(* sort pair array by key that is swapped. *)
let sort_pair pair =
  let comp a b = compare (Pair.key a) (Pair.key b) in
  Array.stable_sort comp pair;
  pair

(* do broad phase *)
let broad_phase engine =
  let pair, swap, count = intersect_bodies engine in
  let pair = sort_pair pair in
  let old_pair = engine.pair.(engine.pair_swap) in
  let old_count = engine.pair_count.(engine.pair_swap) in

  let rec matching_loop newp oldp newc oldc =
    (* TODO: どちらかの配列を超えた場合、処理は終了とする。 *)
    if newc >= count || oldc >= old_count then newp
    else
      let new_key = Pair.key newp.(newc)
      and old_key = Pair.key oldp.(oldc) in
      let c = Int64.compare new_key old_key in
      (* TODO: 同じキーが存在している場合、前の配列のPairをKeepとして更新する *)
      if c = 0 then begin
        newp.(newc) <- Pair.change_pair oldp.(oldc) Pair.Keep;
        matching_loop newp oldp (succ newc) (succ oldc)
      end else if c < 0 then
          matching_loop newp oldp (succ newc) oldc
        else
          matching_loop newp oldp newc (succ oldc) in
  let pair = matching_loop pair old_pair 0 0 in
  engine.pair.(swap) <- pair;
  engine.pair_swap <- swap;
  engine.pair_count.(swap) <- count;
  engine

(* do narrow phase *)
let narrow_phase engine = ()

(* TODO: 拘束計算 *)
let solve_constraints engine = ()


let execute_pipeline engine = ()
