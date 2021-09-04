open Core
module Opt = Engine_option
module State = Realcaml_rigid_body.State
module Sweep_prune = Sweep_prune
module Pair = Realcaml_contact.Pair
module Closest_point = Realcaml_contact.Closest_point
module U = Realcaml_util

type t = {
  engine_option : Opt.t;
  (* manage rigid body informations and collisions for them *)
  sweep_prune : Sweep_prune.t;
  pair_count : int array;
  mutable pair_swap : int;
  mutable pair : Pair.t array array;
}

module RI = Realcaml_rigid_body.Rigid_body_info
module RB = Realcaml_rigid_body.Rigid_body
module A = Typedvec.Algebra
module M = A.Mat
module V = A.Vec
module Q = Typedvec.Ext.Qua

let make ?(option = Opt.empty ()) () =
  {
    engine_option = option;
    sweep_prune = Sweep_prune.make option.Opt.max_bodies;
    pair_swap = 0;
    pair_count = [| 0; 0 |];
    pair = Array.make_matrix ~dimx:2 ~dimy:option.Opt.max_pairs Pair.empty;
  }

let add_body engine body = { engine with sweep_prune = Sweep_prune.add engine.sweep_prune body }

let bodies engine = engine.sweep_prune.Sweep_prune.bodies

(* TRANSLATE: rigid body同士の衝突判定を行う。 *)
let intersect_bodies engine =
  let sp = engine.sweep_prune in
  let max_count = sp.Sweep_prune.current_count in
  let swap = engine.pair_swap lxor 1 in
  let pair_count = ref 0 in
  let pairs = engine.pair.(swap) in
  let rec intersect_loop sp pairs base_count other_count =
    if base_count >= max_count then pairs
    else if base_count = other_count then intersect_loop sp pairs base_count (succ base_count)
    else if other_count >= max_count then intersect_loop sp pairs (succ base_count) (base_count + 2)
    else
      (* TRANSLATE: それぞれの衝突判定を行う。 *)
      match Sweep_prune.intersect sp base_count other_count with
      | None        -> intersect_loop sp pairs base_count (succ other_count)
      | Some (_, _) ->
          (* There are not enough contact point to fill contact module now, so contact information is not set. *)
          let of_int v = Int32.of_int v |> Option.value ~default:0l in
          pairs.(!pair_count) <-
            Pair.make_by_index ~pt:Pair.New ~indexA:(of_int base_count) ~indexB:(of_int other_count) ();
          pair_count := succ !pair_count;
          intersect_loop sp pairs base_count (succ other_count)
  in
  let pairs = intersect_loop sp pairs 0 1 in
  (pairs, swap, !pair_count)

(* sort pair array by key that is swapped. *)
let sort_pair pair =
  let compare a b = Int64.compare a.Pair.key b.Pair.key in
  Array.stable_sort ~compare pair;
  pair

(* do broad phase *)
let broad_phase engine =
  let pair, swap, count = intersect_bodies engine in
  let pair = sort_pair pair in
  let old_pair = engine.pair.(engine.pair_swap) in
  let old_count = engine.pair_count.(engine.pair_swap) in

  let rec matching_loop newp oldp newc oldc =
    (* TRANSLATE: どちらかの配列を超えた場合、処理は終了とする。 *)
    if newc >= count || oldc >= old_count then newp
    else
      let new_key = newp.(newc).Pair.key and old_key = oldp.(oldc).Pair.key in
      let c = Int64.compare new_key old_key in
      (* TRANSLATE: 同じキーが存在している場合、前の配列のPairをKeepとして更新する *)
      if c = 0 then (
        newp.(newc) <- { (oldp.(oldc)) with Pair.pair_type = Pair.Keep };
        matching_loop newp oldp (succ newc) (succ oldc))
      else if c < 0 then matching_loop newp oldp (succ newc) oldc
      else matching_loop newp oldp newc (succ oldc)
  in
  let pair = matching_loop pair old_pair 0 0 in
  engine.pair.(swap) <- pair;
  engine.pair_swap <- swap;
  engine.pair_count.(swap) <- count;
  engine

let update_contact_points bodies closest pair =
  (* TRANSLATE: 最近接点をpairに追加する。 *)
  let module Contact = Realcaml_contact.Contact in
  let to_int v = Int32.to_int v |> Option.value ~default:0 in
  let body_a = bodies.(to_int pair.Pair.indexA) and body_b = bodies.(to_int pair.Pair.indexB) in
  match (body_a, body_b) with
  | None, _ | _, None        -> failwith "body is not found..."
  | Some body_a, Some body_b ->
      { pair with Pair.contact = Contact.update_contact_points pair.Pair.contact ~body_a ~body_b ~closest }

(* do narrow phase *)
let narrow_phase engine =
  (* TRANSLATE: 各ペア同士について、衝突点を計算する *)
  let to_int v = Int32.to_int v |> Option.value ~default:0 in
  let current_pair = engine.pair.(engine.pair_swap) and bodies = engine.sweep_prune.Sweep_prune.bodies in
  let is_separate body_a body_b = Separating_axis.judge_intersect ~body_a ~body_b in

  (* TRANSLATE: あるペアにおける衝突点を計算する *)
  let solve_contact_point pair =
    let body_a = bodies.(to_int pair.Pair.indexA) and body_b = bodies.(to_int pair.Pair.indexB) in
    match (body_a, body_b) with
    | None, _ | _, None        -> failwith "not found one or two of pair"
    | Some body_a, Some body_b -> (
        match is_separate body_a body_b with
        (* TRANSLATE: APlaneの場合、body Aを基準として判定する。 *)
        | Some (Separating_axis.APlane, axis, dist) -> Some (Closest_point.get_closest_point ~axis ~dist body_a body_b)
        (* TRANSLATE: BPlaneの場合、body Bを基準として判定する。 *)
        | Some (Separating_axis.BPlane, axis, dist) -> Some (Closest_point.get_closest_point ~axis ~dist body_b body_a)
        (* TRANSLATE: Edgeの場合、body Aを基準として判定する。 *)
        | Some (Separating_axis.Edge, axis, dist) -> Some (Closest_point.get_closest_point ~axis ~dist body_a body_b)
        (* wTRANSLATE: 分離平面が存在する場合には、このペアに対して何も行わない *)
        | None -> None)
  in

  let updated_pair =
    Array.map
      ~f:(fun pair ->
        match pair.Pair.pair_type with
        | Pair.Empty -> pair
        | _          -> (
            match solve_contact_point pair with
            | None               -> pair
            | Some closest_point -> update_contact_points bodies closest_point pair))
      current_pair
  in
  engine.pair.(engine.pair_swap) <- updated_pair;
  engine

let map_bodies ~f ~motion ary =
  Array.map
    ~f:(function
      | None -> None | Some sp -> ( match motion sp with State.Active -> Some (f sp) | State.Static -> Some sp))
    ary

(* do solve constarints *)
let solve_constraints engine =
  (* TRANSLATE 各Rigid Bodyについて、SolverBodyをセットアップする *)
  let module Solver = Realcaml_contact.Constraint_solver in
  let to_int v = Int32.to_int v |> Option.value ~default:0 in
  let current_pair = engine.pair.(engine.pair_swap) and bodies = engine.sweep_prune.Sweep_prune.bodies in
  let setup_solver = function Some o -> Some (Solver.setup_solver_body o) | None -> None in
  let solver_bodies = Array.map ~f:setup_solver bodies in
  let solver_set ind = solver_bodies.(to_int ind) in
  let update_contact pair =
    match pair.Pair.pair_type with
    | Pair.Empty -> pair
    | _          -> (
        match (solver_set pair.Pair.indexA, solver_set pair.Pair.indexB) with
        | None, _ | _, None        -> pair
        | Some solv_a, Some solv_b ->
            let new_contact =
              Solver.setup_constraint solv_a solv_b pair.Pair.contact pair.Pair.pair_type
                {
                  Solver.contact_bias = engine.engine_option.Opt.contact_bias;
                  time_step = engine.engine_option.Opt.time_step;
                }
            in
            { pair with Pair.contact = new_contact })
  in

  let do_solve pair =
    match pair.Pair.pair_type with
    | Pair.Empty -> ()
    | _          -> (
        match (solver_set pair.Pair.indexA, solver_set pair.Pair.indexB) with
        | None, _ | _, None        -> ()
        | Some solv_a, Some solv_b ->
            let (bodyA, solverA), (bodyB, solverB) = Solver.solve solv_a solv_b pair.Pair.contact in
            let indA = Option.value_exn (Int32.to_int pair.Pair.indexA)
            and indB = Option.value_exn (Int32.to_int pair.Pair.indexB) in
            solver_bodies.(indA) <- Some (bodyA, solverA);
            solver_bodies.(indB) <- Some (bodyB, solverB))
  in

  let update_velocity body =
    match body with
    | None                -> None
    | Some (body, solver) ->
        let state = body.RI.state in
        let module S = Solver.Solver_body in
        let linear = V.add state.State.linear_velocity solver.S.delta_linear_velocity
        and angular = V.add state.State.angular_velocity solver.S.delta_angular_velocity in
        let state = { state with State.linear_velocity = linear; angular_velocity = angular } in
        Some { body with RI.state }
  in

  (* TRANSLATE 各pairについて、拘束のセットアップを行う *)
  let current_pair = Array.map ~f:update_contact current_pair in
  (* TRANSLATE 各pairについて、拘束力の演算を行う *)
  Array.iter ~f:do_solve current_pair;
  (* TRANSLATE 算出した拘束力を更新する *)
  let bodies = Array.map ~f:update_velocity solver_bodies in

  (* TRANSLATE 更新したSweepPluneを、engineに再度設定する *)
  engine.pair.(engine.pair_swap) <- current_pair;
  { engine with sweep_prune = { engine.sweep_prune with Sweep_prune.bodies } }

let update_bodies engine =
  (* TRANSLATE: 算出した速度を、各剛体に反映する' *)
  let time_step = engine.engine_option.Opt.time_step and bodies = engine.sweep_prune.Sweep_prune.bodies in

  let calc_delta_orientation angular time_step =
    let angular = V.scalar angular ~scale:time_step in
    let axis = V.scalar angular ~scale:(1.0 /. V.norm angular) in
    Q.make ~angle:(V.norm angular) ~axis ()
  in

  let update_state_position ri =
    let state = ri.RI.state in
    let pos = state.State.pos in
    let state = { state with State.pos = V.add pos (V.scalar ~scale:time_step state.State.linear_velocity) } in
    let delta = calc_delta_orientation state.State.angular_velocity time_step in
    let state = { state with State.orientation = Q.multiply state.State.orientation delta } in
    { ri with RI.state }
  in

  {
    engine with
    sweep_prune =
      {
        engine.sweep_prune with
        Sweep_prune.bodies =
          map_bodies ~f:update_state_position ~motion:(fun sp -> sp.RI.state.State.motion_type) bodies;
      };
  }

(* 各剛体に重力の形で外力を与える。それぞれの剛体は並進運動を行う *)
let apply_gravity engine =
  let bodies = engine.sweep_prune.Sweep_prune.bodies in
  let updated =
    Array.map
      ~f:(fun body ->
        match body with
        | None      -> None
        | Some body ->
            if State.is_active body.RI.state then
              let mass = body.RI.body.RB.mass in
              let gravity = engine.engine_option.Opt.gravity and time_step = engine.engine_option.Opt.time_step in
              let force = V.scalar gravity ~scale:mass in
              let ri_body, state =
                Force.apply_force ~body:body.RI.body ~state:body.RI.state ~force ~torque:(U.Vec.empty ()) ~time_step ()
              in
              Some { body with RI.state; RI.body = ri_body }
            else Some body)
      bodies
  in
  { engine with sweep_prune = { engine.sweep_prune with Sweep_prune.bodies = updated } }

let execute_pipeline engine =
  (* TRANSLATE: 剛体に重力の形で外力を与える *)
  apply_gravity engine |> broad_phase |> narrow_phase |> solve_constraints |> update_bodies
