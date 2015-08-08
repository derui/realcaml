module V = Candyvec.Std.Vector
module M = Candyvec.Std.Matrix
module Q = Candyvec.Std.Quaternion

open Sugarpot.Std.Prelude

(**
   Contain for solving constraint between rigid bodies.
*)
module SolverBody = struct

  (** type of the SolverBody module  *)
  type t = {
    delta_linear_velocity: V.t; (* translate 速度の差分 *)
    delta_angular_velocity: V.t; (* translate 角速度の差分 *)
    orientation:Q.t; (* 拘束の上での向き *)
    inertia_inv:M.t; (* 慣性の逆行列 *)
    mass_inv:float;  (* 質量の逆数 *)
    accum_impulse : float;
  }

  let empty = {
    delta_linear_velocity = V.zero;
    delta_angular_velocity = V.zero;
    orientation = Q.identity;
    inertia_inv = M.identity ();
    mass_inv = 0.0;
    accum_impulse = 0.0;
  }

  (** Set up new solver body with a state of the rigid body *)
  let setup state body =
    let orient_mat = Q.to_matrix state.State.orientation in
    let (inertia, mass) = match state.State.motion_type with
      | State.Static -> (M.scaling V.zero, 0.0)
      | State.Active ->
        let open Sugarpot.Std.Prelude in
        let inversed = M.inverse body.RigidBody.inertia in
        match inversed with
        | Some inversed ->
          let open M.Open in
          (orient_mat *|> inversed *|> (M.transpose orient_mat),
           1.0 /. body.RigidBody.mass)
        | None -> failwith "can't compute inversed matrix from inertia" in

    {delta_linear_velocity = V.zero;
     delta_angular_velocity = V.zero;
     orientation = state.State.orientation;
     inertia_inv = inertia;
     mass_inv = mass;
     accum_impulse = 0.0;
    }
end

let setup_solver_body bi =
  let state = bi.RigidBodyInfo.state
  and body = bi.RigidBodyInfo.body in
  (bi, SolverBody.setup state body)

type solver_info = RigidBodyInfo.t * SolverBody.t

let calc_velocity s r =
  V.add s.State.linear_velocity (V.cross s.State.angular_velocity r)

let calc_restriction bodyA bodyB = function
  | Pair.New ->
    let open RigidBodyInfo in
    let open RigidBody in
    0.5 *. (bodyA.body.friction +. bodyB.body.friction)
  | _ -> 0.0

let calc_k (bodyA, solverA) (bodyB, solverB) cp =
  let open State in
  let open RigidBodyInfo in
  let open ContactPoint in
  let rA = Q.rotate bodyA.state.orientation cp.pointA
  and rB = Q.rotate bodyB.state.orientation cp.pointB in
  let scale = M.scaling {V.x = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;
                         V.y = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;
                         V.z = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;} in
  let cross_a = V.cross_matrix rA
  and cross_b = V.cross_matrix rB in
  let open M.Open in
  let k_a = cross_a *|> solverA.SolverBody.inertia_inv *|> cross_a
  and k_b = cross_b *|> solverB.SolverBody.inertia_inv *|> cross_b in
  M.subtract (M.subtract scale k_a) k_b

let setup_constraint (bodyA, solverA) (bodyB, solverB) contact pair_type opt =
  let open State in
  let open RigidBody in
  let open RigidBodyInfo in
  let friction = sqrt (bodyA.body.friction *. bodyB.body.friction) in
  let setup_contact_point cp =
    let open ContactPoint in
    let rA = Q.rotate bodyA.state.orientation cp.pointA
    and rB = Q.rotate bodyB.state.orientation cp.pointB in
    let velocityA = calc_velocity bodyA.state rA
    and velocityB = calc_velocity bodyB.state rB in

    let open V.Open in
    let relative_velocity = velocityA -@ velocityB in
    let restriction = calc_restriction bodyA bodyB pair_type in
    let k = calc_k (bodyA, solverA) (bodyB, solverB) cp in

    let open M.Open in
    let open Engine_option in
    let module CS = Constraint.Setup in
    let info = {CS.k; restriction; relative_velocity;
                contact_bias = opt.contact_bias; distance = cp.ContactPoint.distance;
                time_step = opt.time_step} in
    let vec = V.add cp.normal ({V.x = 1.0;y = 0.0;z = 0.0} +@ cp.normal) in
    let tangent1 = V.normalize (cp.normal **@ vec) in
    let tangent2 = V.normalize (tangent1 **@ cp.normal) in
    let constraint1 = Constraint.make info cp.normal
    and constraint2 = Constraint.make info tangent1
    and constraint3 = Constraint.make info tangent2 in
    {cp  with ContactPoint.constraints = [constraint1;constraint2; constraint3]} in
  let setuped = List.map setup_contact_point contact.Contact.contact_points in
  {contact with Contact.contact_points = setuped; friction}

module Solver = struct
  (* translate: 撃力について、衝突ベクトルとそれぞれの速度から算出する *)
  let calc_delta impulse ct va vb =
    let open V.Open in
    let jac = ct.Constraint.jac_diag_inv
    and axis = ct.Constraint.axis in
    let impulse = impulse -. jac *. (axis *@ (va -@ vb)) in

    let low_lim = ct.Constraint.lower_limit
    and up_lim = ct.Constraint.upper_limit in
    let clamp v mx mn = max v mn |> min mx in
    (* translate: アキュムレートインパルスから、撃力の安定化を行えるように *)
    let impulse = clamp impulse low_lim up_lim in
    let old_impulse = ct.Constraint.accum_impulse in
    let accum_impulse = clamp (old_impulse +. impulse) low_lim up_lim in
    (accum_impulse -. old_impulse, impulse)

  (* translate: Solverを更新する *)
  let update_solver (impulse, delta) solver ct r operate =
    let open V.Open in
    let scale = impulse *. solver.SolverBody.mass_inv in
    let inertia = M.ratio solver.SolverBody.inertia_inv impulse in
    let cross = ct.Constraint.axis **@ r in
    let calc_vec f =
      {solver with
        SolverBody.accum_impulse = delta;
        delta_linear_velocity = f solver.SolverBody.delta_linear_velocity
          (V.scale ~v:ct.Constraint.axis ~scale);
        delta_angular_velocity = f solver.SolverBody.delta_angular_velocity
          (M.mult_vec ~mat:inertia ~vec:cross);
      } in
    match operate with
    | `Add -> calc_vec V.add
    | `Minus -> calc_vec V.sub

end

(* TODO 渡されたbodyとsolverに対して、拘束力の算出を行う *)
let solve (bodyA, solverA) (bodyB, solverB) contact opt =

  let cps = contact.Contact.contact_points in

  (* TRANSLATE 特定のContactPointについて、拘束演算を行う *)
  let solve_per_contact_point solverA solverB cp =
    let module S = SolverBody in
    let open RigidBodyInfo in
    let open State in
    let rA = Q.rotate bodyA.state.orientation cp.ContactPoint.pointA
    and rB = Q.rotate bodyB.state.orientation cp.ContactPoint.pointB in

    let open Sugarpot.Std.Option.Open in
    let open Sugarpot.Std.List in
    safe_hd cp.ContactPoint.constraints >>=
      (fun ctraint ->
        let delta_impulse = ctraint.Constraint.rhs in
        let delta_velocity_a = V.add solverA.S.delta_linear_velocity (V.cross solverA.S.delta_angular_velocity rA)
        and delta_velocity_b = V.add solverB.S.delta_linear_velocity (V.cross solverB.S.delta_angular_velocity rB) in
        let delta_impulse, accum_impulse_delta =
          Solver.calc_delta delta_impulse ctraint delta_velocity_a delta_velocity_b in
        let solverA = Solver.update_solver (delta_impulse, accum_impulse_delta) solverA ctraint rA `Add
        and solverB = Solver.update_solver (delta_impulse, accum_impulse_delta) solverB ctraint rB `Minus in
        return (solverA, solverB)) in

  let solve_for_contact solverA solverB =
    List.fold_left (fun solvers cp ->
      match solvers with
      | None -> failwith "Not have any constraints, should initialize constraints."
      | Some (sA, sB) ->
        solve_per_contact_point sA sB cp) (Some (solverA, solverB)) in
  let (solverA, solverB) = Sugarpot.Std.Option.get (solve_for_contact solverA solverB cps) in
  ((bodyA, solverA), (bodyB, solverB))

let update_constraint contact solver =
  let cps = contact.Contact.contact_points in
  let friction = contact.Contact.friction in

  let new_constraint ct fric = {ct with Constraint.lower_limit = -.fric; upper_limit = fric} in

  let update_constraint_for_cp cp =
    match cp.ContactPoint.constraints with
    | ctraint1 :: ctraint2 :: ctraint3 :: _ ->
      let max_friction = friction *. (abs_float ctraint1.Constraint.accum_impulse) in
      {cp with ContactPoint.constraints =
          [ctraint1;
           new_constraint ctraint1 max_friction;
           new_constraint ctraint2 max_friction]}
    | _ -> cp in
  let updated_cps = List.map update_constraint_for_cp cps in
  {contact with Contact.contact_points = updated_cps}
