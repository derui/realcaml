open Core
module A = Typedvec.Algebra
module V = A.Vec
module M = A.Mat
module S = Typedvec.Size
module Q = Typedvec.Ext.Qua
module U = Realcaml_util

type vec = Realcaml_util.Types.vec

type mat = Realcaml_util.Types.mat

(* Options to setup constraint solver. *)
type constraint_option = {
  contact_bias : float;
  time_step : float;
}

(** Contain for solving constraint between rigid bodies. *)
module Solver_body = struct
  type t = {
    delta_linear_velocity : vec;
    (* translate 速度の差分 *)
    delta_angular_velocity : vec;
    (* translate 角速度の差分 *)
    orientation : Q.t;
    (* 拘束の上での向き *)
    inertia_inv : mat;
    (* 慣性の逆行列 *)
    mass_inv : float;
    (* 質量の逆数 *)
    accum_impulse : float;
  }
  (** type of the SolverBody module *)

  let empty =
    {
      delta_linear_velocity = U.Vec.empty ();
      delta_angular_velocity = U.Vec.empty ();
      orientation = Q.identity;
      inertia_inv = M.identity Typedvec.Size.four;
      mass_inv = 0.0;
      accum_impulse = 0.0;
    }

  (** Set up new solver body with a state of the rigid body *)
  let setup ~state ~body () =
    let open Realcaml_rigid_body in
    let orient_mat = Q.to_mat state.State.orientation in
    let inertia, mass =
      match state.State.motion_type with
      | State.Static -> (M.identity Typedvec.Size.four, 0.0)
      | State.Active -> (
          let inversed = M.inverse body.Rigid_body.inertia in
          match inversed with
          | Some inversed ->
              let open M.Open in
              let module AF = Typedvec.Ext.Affine in
              let affine_normal = AF.make Typedvec.Size.three in
              let affine_normal = AF.rotate affine_normal ~rotate:orient_mat in
              let affine_transpose = AF.make Typedvec.Size.three in
              let affine_transpose = AF.rotate affine_transpose ~rotate:(M.transpose orient_mat) in
              (AF.to_mat affine_normal *: inversed *: AF.to_mat affine_transpose, 1.0 /. body.Rigid_body.mass)
          | None          -> failwith "can't compute inversed matrix from inertia")
    in

    {
      delta_linear_velocity = U.Vec.empty ();
      delta_angular_velocity = U.Vec.empty ();
      orientation = state.State.orientation;
      inertia_inv = inertia;
      mass_inv = mass;
      accum_impulse = 0.0;
    }
end

let setup_solver_body bi =
  let open Realcaml_rigid_body in
  let state = bi.Rigid_body_info.state and body = bi.Rigid_body_info.body in
  (bi, Solver_body.setup ~state ~body ())

type solver_info = Realcaml_rigid_body.Rigid_body_info.t * Solver_body.t

let calc_velocity s r =
  let open Realcaml_rigid_body in
  let open V.Open in
  s.State.linear_velocity +: V.cross ~left:s.State.angular_velocity ~right:r

let calc_restriction bodyA bodyB = function
  | Pair.New ->
      let open Realcaml_rigid_body in
      let open Rigid_body_info in
      let open Rigid_body in
      0.5 *. (bodyA.body.friction +. bodyB.body.friction)
  | _        -> 0.0

let calc_k (bodyA, solverA) (bodyB, solverB) cp =
  let open Realcaml_rigid_body in
  let open State in
  let open Rigid_body_info in
  let open Contact_point in
  let open A.Open in
  let rA = Q.to_mat bodyA.state.orientation *< cp.pointA and rB = Q.to_mat bodyB.state.orientation *< cp.pointB in

  let module AF = Typedvec.Ext.Affine in
  let scale = M.diagonal Typedvec.Size.four ~comp:(solverA.Solver_body.mass_inv +. solverB.Solver_body.mass_inv) in
  (* それぞれをAffine変換を通して4x4に変換する *)
  let cross_a = AF.make S.three |> AF.rotate ~rotate:(A.cross_to_mat rA) |> AF.to_mat
  and cross_b = AF.make S.three |> AF.rotate ~rotate:(A.cross_to_mat rB) |> AF.to_mat in
  let open M.Open in
  let k_a = cross_a *: solverA.Solver_body.inertia_inv *: cross_a
  and k_b = cross_b *: solverB.Solver_body.inertia_inv *: cross_b in
  scale -: k_a -: k_b

let setup_constraint (bodyA, solverA) (bodyB, solverB) contact pair_type opt =
  let open Realcaml_rigid_body in
  let open State in
  let open Rigid_body in
  let open Rigid_body_info in
  let friction = sqrt (bodyA.body.friction *. bodyB.body.friction) in
  let setup_contact_point cp =
    let open Contact_point in
    let to_r q p =
      let open A.Open in
      Q.to_mat q *< p
    in
    let rA = to_r bodyA.state.orientation cp.pointA and rB = to_r bodyB.state.orientation cp.pointB in
    let velocityA = calc_velocity bodyA.state rA and velocityB = calc_velocity bodyB.state rB in

    let open V.Open in
    let relative_velocity = velocityA -: velocityB in
    let restriction = calc_restriction bodyA bodyB pair_type in
    let k = calc_k (bodyA, solverA) (bodyB, solverB) cp in

    let module CP = Contact_point in
    let info =
      {
        Setup.k;
        restriction;
        relative_velocity;
        contact_bias = opt.contact_bias;
        distance = cp.CP.distance;
        time_step = opt.time_step;
      }
    in
    let vec = cp.normal +: (U.Vec.unit `X +: cp.normal) in
    let tangent1 = V.cross ~left:cp.normal ~right:vec |> V.normalize in
    let tangent2 = V.cross ~left:tangent1 ~right:cp.normal |> V.normalize in
    let constraint1 = Constraint.make info cp.normal
    and constraint2 = Constraint.make info tangent1
    and constraint3 = Constraint.make info tangent2 in
    { cp with CP.constraints = [ constraint1; constraint2; constraint3 ] }
  in
  let setuped = List.map ~f:setup_contact_point contact.Contact.contact_points in
  { contact with Contact.contact_points = setuped; friction }

module Solver = struct
  (* translate: 撃力について、衝突ベクトルとそれぞれの速度から算出する *)
  let calc_delta impulse ct va vb =
    let open V.Open in
    let jac = ct.Constraint.jac_diag_inv and axis = ct.Constraint.axis in
    let impulse = impulse -. (jac *. (axis *: (va -: vb))) in

    let low_lim = ct.Constraint.lower_limit and up_lim = ct.Constraint.upper_limit in
    let clamp v mx mn = Float.max v mn |> Float.min mx in
    (* translate: アキュムレートインパルスから、撃力の安定化を行えるように *)
    let impulse = clamp impulse low_lim up_lim in
    let old_impulse = ct.Constraint.accum_impulse in
    let accum_impulse = clamp (old_impulse +. impulse) low_lim up_lim in
    (accum_impulse -. old_impulse, impulse)

  (* translate: Solverを更新する *)
  let update_solver (impulse, delta) solver ct r operate =
    let scale = impulse *. solver.Solver_body.mass_inv in
    let inertia = M.scalar solver.Solver_body.inertia_inv ~scale:impulse in
    let cross = V.cross ~left:ct.Constraint.axis ~right:r in
    let calc_vec f =
      {
        solver with
        Solver_body.accum_impulse = delta;
        delta_linear_velocity = f solver.Solver_body.delta_linear_velocity (V.scalar ct.Constraint.axis ~scale);
        delta_angular_velocity =
          f solver.Solver_body.delta_angular_velocity (U.Vec.to_four cross |> A.mul_m2v inertia |> U.Vec.to_three);
      }
    in
    match operate with `Add -> calc_vec V.add | `Minus -> calc_vec V.sub
end

(* TODO 渡されたbodyとsolverに対して、拘束力の算出を行う *)
let solve (bodyA, solverA) (bodyB, solverB) contact =
  let cps = contact.Contact.contact_points in

  (* TRANSLATE 特定のContactPointについて、拘束演算を行う *)
  let solve_per_contact_point solverA solverB cp =
    let module S = Solver_body in
    let open Realcaml_rigid_body.Rigid_body_info in
    let open Realcaml_rigid_body.State in
    let open A.Open in
    let rA = Q.to_mat bodyA.state.orientation *< cp.Contact_point.pointA
    and rB = Q.to_mat bodyB.state.orientation *< cp.Contact_point.pointB in

    let open Option in
    List.hd cp.Contact_point.constraints >>= fun ctraint ->
    let delta_impulse = ctraint.Constraint.rhs in
    let delta_velocity_a =
      V.add solverA.S.delta_linear_velocity (V.cross ~left:solverA.S.delta_angular_velocity ~right:rA)
    and delta_velocity_b =
      V.add solverB.S.delta_linear_velocity (V.cross ~left:solverB.S.delta_angular_velocity ~right:rB)
    in
    let delta_impulse, accum_impulse_delta =
      Solver.calc_delta delta_impulse ctraint delta_velocity_a delta_velocity_b
    in
    let solverA = Solver.update_solver (delta_impulse, accum_impulse_delta) solverA ctraint rA `Add
    and solverB = Solver.update_solver (delta_impulse, accum_impulse_delta) solverB ctraint rB `Minus in
    return (solverA, solverB)
  in

  let solve_for_contact solverA solverB =
    List.fold_left
      ~f:(fun solvers cp ->
        match solvers with
        | None          -> failwith "Not have any constraints, should initialize constraints."
        | Some (sA, sB) -> solve_per_contact_point sA sB cp)
      ~init:(Some (solverA, solverB))
  in
  let solverA, solverB = Option.value_exn (solve_for_contact solverA solverB cps) in
  ((bodyA, solverA), (bodyB, solverB))

let update_constraint contact _ =
  let module CP = Contact_point in
  let cps = contact.Contact.contact_points in
  let friction = contact.Contact.friction in

  let new_constraint ct fric = { ct with Constraint.lower_limit = -.fric; upper_limit = fric } in

  let update_constraint_for_cp cp =
    match cp.CP.constraints with
    | ctraint1 :: ctraint2 :: _ :: _ ->
        let max_friction = friction *. Float.abs ctraint1.Constraint.accum_impulse in
        {
          cp with
          CP.constraints = [ ctraint1; new_constraint ctraint1 max_friction; new_constraint ctraint2 max_friction ];
        }
    | _                              -> cp
  in
  let updated_cps = List.map ~f:update_constraint_for_cp cps in
  { contact with Contact.contact_points = updated_cps }
