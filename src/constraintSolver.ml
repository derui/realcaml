module V = Candyvec.Std.Vector
module M = Candyvec.Std.Matrix4
module M3 = Candyvec.Std.Matrix3
module Q = Candyvec.Std.Quaternion

open Sugarpot.Std.Prelude

(**
   Contain for solving constraint between rigid bodies.
*)
module SolverBody = struct

  (** type of the SolverBody module  *)
  type t = {delta_linear_velocity: V.t;
            delta_angular_velocity: V.t;
            orientation:Q.t;
            inertia_inv:Candyvec.Matrix3.t;
            mass_inv:float;
           }

  let empty = {delta_linear_velocity = V.zero;
            delta_angular_velocity = V.zero;
            orientation = Q.identity;
            inertia_inv = Candyvec.Std.Matrix3.identity ();
            mass_inv = 0.0;
           }

  (** Set up new solver body with a state of the rigid body *)
  let setup state body =
    let module MU = Candyvec.Std.Matrix in
    let orient_mat = Q.to_matrix state.State.orientation in
    let (inertia, mass) = match state.State.motion_type with
      | State.Static -> (M.scaling V.zero, 0.0)
      | State.Active ->
        let open Sugarpot.Std.Prelude in
        let transposed = Candyvec.Std.Matrix3.transpose body.RigidBody.inertia in
        (M.multiply (M.multiply orient_mat
                       (Candyvec.Std.MatrixUtil.to_4x4 transposed))
        (M.transpose orient_mat), 1.0 /. body.RigidBody.mass) in

    {delta_linear_velocity = V.zero;
     delta_angular_velocity = V.zero;
     orientation = state.State.orientation;
     inertia_inv = MU.to_3x3 inertia; mass_inv = mass;
    }
  ;;
end

let setup_solver_body bi =
  let state = bi.RigidBodyInfo.state
  and body = bi.RigidBodyInfo.body in
  SolverBody.setup state body

type solver_info = RigidBodyInfo.t * SolverBody.t

let setup_constraint (bodyA, solverA) (bodyB, solverB) contact pair_type opt =
  let calc_velocity s r = V.add s.State.linear_velocity (V.cross s.State.angular_velocity r) in

  let setup_contact_point cp =
    let rA = Q.rotate bodyA.RigidBodyInfo.state.State.orientation cp.ContactPoint.pointA
    and rB = Q.rotate bodyB.RigidBodyInfo.state.State.orientation cp.ContactPoint.pointB in
    let velocityA = calc_velocity bodyA.RigidBodyInfo.state rA
    and velocityB = calc_velocity bodyB.RigidBodyInfo.state rB in

    let relative_velocity = V.sub velocityA velocityB in
    let module M3 = Candyvec.Std.Matrix3 in
    let scale = M3.scaling {V.x = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;
                            V.y = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;
                            V.z = solverA.SolverBody.mass_inv +. solverB.SolverBody.mass_inv;} in
    let cross_a = V.cross_matrix rA
    and cross_b = V.cross_matrix rB in
    let k_a = M3.multiply (M3.multiply cross_a solverA.SolverBody.inertia_inv) cross_a
    and k_b = M3.multiply (M3.multiply cross_b solverB.SolverBody.inertia_inv) cross_b in
    let k = M3.subtract (M3.subtract scale k_a) k_b in

    let restriction = match pair_type with
      | Pair.New -> 0.5 *. bodyA.RigidBodyInfo.body.RigidBody.friction *.
        bodyB.RigidBodyInfo.body.RigidBody.friction
      | Pair.Keep -> 0.0
      | Pair.Empty -> 0.0
    in
    let open Engine_option in
    let setup axis = let denom = V.dot (M3.mult_vec k axis) axis in
                     let rhs = -.(1.0 +. restriction) *. (V.dot relative_velocity axis) in
                     Printf.printf "initial rhs : %f\n" (min 0.0 cp.ContactPoint.distance);
                     let rhs = rhs -.
                       (opt.contact_bias *. min 0.0 cp.ContactPoint.distance) /. opt.time_step in
                     Printf.printf "calculated rhs : %f\n" rhs;
                     let rhs = rhs *. 1.0 /. denom in
                     {Constraint.axis; jac_diag_inv = 1.0 /. denom;
                      rhs; lower_limit = 0.0;
                      upper_limit = max_float;
                      accum_impulse = 0.0
                     } in
    let vec = V.add cp.ContactPoint.normal {V.x = 1.0; y = 1.0; z = 1.0 } in
    let tangent1 = V.normalize (V.cross cp.ContactPoint.normal vec) in
    let tangent2 = V.normalize (V.cross tangent1 cp.ContactPoint.normal) in
    let constraint1 = setup cp.ContactPoint.normal
    and constraint2 = setup tangent1
    and constraint3 = setup tangent2 in
    {cp  with ContactPoint.constraints = [constraint1;constraint2; constraint3]} in
  let setuped = List.map setup_contact_point contact.Contact.contact_points in
  {contact with Contact.contact_points = setuped}
;;

(* TODO 渡されたbodyとsolverに対して、拘束力の算出を行う *)
let solve (bodyA, solverA) (bodyB, solverB) contact opt =
  let calc_delta impulse ct va vb =
    let jac = ct.Constraint.jac_diag_inv
    and axis = ct.Constraint.axis in
    let impulse = impulse -. jac *. (V.dot axis (V.sub va vb)) in
    max impulse ct.Constraint.lower_limit |> min ct.Constraint.upper_limit in

  let update_solver impulse solver ct r operate =
    let scale = impulse *. solver.SolverBody.mass_inv in
    let inertia = M3.ratio solver.SolverBody.inertia_inv impulse in
    let cross = V.cross r ct.Constraint.axis in
    Printf.printf  "scale : %f\n" scale;
    Printf.printf  "cross : %s %s\n" (V.to_string r) (V.to_string cross);

    Printf.printf  "axis : %s\n" (V.to_string ct.Constraint.axis) ;
    let calc_vec f =
      {solver with SolverBody.delta_linear_velocity =
          f solver.SolverBody.delta_linear_velocity
            (V.scale ~v:ct.Constraint.axis ~scale);
        delta_angular_velocity =
          f solver.SolverBody.delta_angular_velocity
            (M3.mult_vec ~mat:inertia ~vec:cross);
      } in
    match operate with
    | `Add -> calc_vec V.add
    | `Minus -> calc_vec V.sub in

  let cps = contact.Contact.contact_points in
  
  (* TRANSLATE 特定のContactPointについて、拘束演算を行う *)
  let solve_per_contact_point solverA solverB cp =
    let module S = SolverBody in
    let rA = Q.rotate bodyA.RigidBodyInfo.state.State.orientation cp.ContactPoint.pointA
    and rB = Q.rotate bodyB.RigidBodyInfo.state.State.orientation cp.ContactPoint.pointB in
  
    let open Sugarpot.Std.Option.Open in
    let open Sugarpot.Std.List in
    safe_hd cp.ContactPoint.constraints >>=
      (fun ctraint -> 
        let delta_impulse = ctraint.Constraint.rhs in
        Printf.printf "initial delta of impulse : %f\n" delta_impulse;
        let delta_velocity_a = V.add solverA.S.delta_linear_velocity (V.cross solverA.S.delta_angular_velocity rA)
        and delta_velocity_b = V.add solverB.S.delta_linear_velocity (V.cross solverB.S.delta_angular_velocity rB) in
        let delta_impulse = calc_delta delta_impulse ctraint delta_velocity_a delta_velocity_b in
        Printf.printf "delta of impulse : %f\n" delta_impulse;
        let solverA = update_solver delta_impulse solverA ctraint rA `Add
        and solverB = update_solver delta_impulse solverB ctraint rB `Minus in
        Printf.printf "solver A : %s\n" (V.to_string (solverA.S.delta_linear_velocity));
        Printf.printf "solver B : %s\n" (V.to_string (solverB.S.delta_linear_velocity));
        return (solverA, solverB)) in

  let solve_for_contact solverA solverB =
    List.fold_left (fun solvers cp ->
      match solvers with
      | None -> failwith "Not have any constraints, maybe not initialized?"
      | Some (sA, sB) -> 
        solve_per_contact_point sA sB cp) (Some (solverA, solverB)) in
  let (solverA, solverB) = Sugarpot.Std.Option.get (solve_for_contact solverA solverB cps) in
  ((bodyA, solverA), (bodyB, solverB))
;;

let update_constraint contact =
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
;;
