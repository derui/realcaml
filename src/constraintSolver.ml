module V = Vecmath.Std.Vector
module M = Vecmath.Std.Matrix4
module M3 = Vecmath.Std.Matrix3
module Q = Vecmath.Std.Quaternion

open Baselib.Std.Prelude

(**
   Contain for solving constraint between rigid bodies.
*)
module SolverBody = struct

  (** type of the SolverBody module  *)
  type t = {delta_linear_velocity: V.t;
            delta_angular_velocity: V.t;
            orientation:Q.t;
            inertia_inv:Vecmath.Matrix3.t;
            mass_inv:float;
           }

  let empty = {delta_linear_velocity = V.zero;
            delta_angular_velocity = V.zero;
            orientation = Q.identity ();
            inertia_inv = Vecmath.Matrix3.identity ();
            mass_inv = 0.0;
           }

  (** Set up new solver body with a state of the rigid body *)
  let setup state body =
    let orient_mat = Q.to_matrix (State.orientation state) in
    let (inertia, mass) = match State.motion_type state with
      | State.Static -> (M.scaling V.zero, 0.0)
      | State.Active ->
        let open Baselib.Std.Prelude in
        let transposed = Vecmath.Matrix3.transpose |< RigidBody.inertia body in
        (M.multiply (M.multiply orient_mat
                       (Vecmath.Matrix_util.to_4x4 transposed))
        (M.transpose orient_mat), 1.0 /. RigidBody.mass body) in

    {delta_linear_velocity = V.zero;
     delta_angular_velocity = V.zero;
     orientation = State.orientation state;
     inertia_inv = M.upper3x3 inertia; mass_inv = mass;
    }
  ;;

  (** get delta of linear velocity of given solver body *)
  let delta_linear_velocity {delta_linear_velocity;_} = delta_linear_velocity

  (** get delta of angular velocity of given solver body *)
  let delta_angular_velocity {delta_angular_velocity;_} = delta_angular_velocity

  (** get the orientation of given solver body *)
  let orientation {orientation;_} = orientation

  (** get the inverse for inertia of given solver body  *)
  let inertia_inv {inertia_inv;_} = inertia_inv

  (** get the inverse for mass of given solver body *)
  let mass_inv {mass_inv;_} = mass_inv
end

let setup_solver_body bi =
  let state = RigidBodyInfo.state bi
  and body = RigidBodyInfo.rigid_body bi in
  SolverBody.setup state body

type solver_info = RigidBodyInfo.t * SolverBody.t

let setup_constraint (bodyA, solverA) (bodyB, solverB) contact pair_type opt =
  let calc_velocity s r = V.add (State.linear_velocity s) (V.cross (State.angular_velocity s) r) in
  let rA = Q.rotate (State.orientation (RigidBodyInfo.state bodyA)) (ContactPoint.pointA contact)
  and rB = Q.rotate (State.orientation (RigidBodyInfo.state bodyB)) (ContactPoint.pointB contact) in
  let velocityA = calc_velocity (RigidBodyInfo.state bodyA) rA
  and velocityB = calc_velocity (RigidBodyInfo.state bodyB) rB in
  let relative_velocity = V.sub velocityA velocityB in
  let module M3 = Vecmath.Matrix3 in
  let scale = M3.scaling {V.x = (SolverBody.mass_inv solverA) +. (SolverBody.mass_inv solverB);
                         V.y = (SolverBody.mass_inv solverA) +. (SolverBody.mass_inv solverB);
                         V.z = (SolverBody.mass_inv solverA) +. (SolverBody.mass_inv solverB);} in
  let cross_a = V.cross_matrix rA
  and cross_b = V.cross_matrix rB in
  let k_a = M3.multiply (M3.multiply cross_a (SolverBody.inertia_inv solverA)) cross_a
  and k_b = M3.multiply (M3.multiply cross_b (SolverBody.inertia_inv solverB)) cross_b in
  let k = M3.subtract (M3.subtract scale k_a) k_b in

  let restriction = match pair_type with
    | Pair.New -> 0.5 *. (RigidBody.friction (RigidBodyInfo.rigid_body bodyA)) *.
      (RigidBody.friction (RigidBodyInfo.rigid_body bodyB))
    | Pair.Keep -> 0.0 in

  let open Engine_option in
  let setup axis = let denom = V.dot (M3.mult_vec k axis) axis in
                   let rhs = -.(1.0 +. restriction) *. (V.dot relative_velocity axis) in
                   let rhs = rhs -.
                     (opt.contact_bias *. (min 0.0 |< ContactPoint.distance contact)) /. opt.time_step in
                   let rhs = rhs *. 1.0 /. denom in
                   Constraint.make ~axis ~inv:(1.0 /. denom) ~rhs ~lower:0.0 ~upper:max_float ~accum:0.0 in
  let tangent1 = V.normalize (V.add (V.normal_axis `X) (ContactPoint.normal contact)) in
  let tangent2 = V.normalize (V.add tangent1 (ContactPoint.normal contact)) in
  let constraint1 = setup (ContactPoint.normal contact)
  and constraint2 = setup tangent1
  and constraint3 = setup tangent2 in
  let constraints = ContactPoint.constraints contact in
  constraints.(0) <- constraint1;
  constraints.(1) <- constraint2;
  constraints.(2) <- constraint3;
  contact
;;

(* TODO 渡されたbodyとsolverに対して、拘束力の算出を行う *)
let solve (bodyA, solverA) (bodyB, solverB) contact opt =
  let calc_velocity s r = V.add (State.linear_velocity s) (V.cross (State.angular_velocity s) r) in
  let rA = Q.rotate (State.orientation (RigidBodyInfo.state bodyA)) (ContactPoint.pointA contact)
  and rB = Q.rotate (State.orientation (RigidBodyInfo.state bodyB)) (ContactPoint.pointB contact) in
  let friction = 

  let calc_delta impulse ct va vb =
    let jac = Constraint.jac_diag_inv ct
    and axis = (Constraint.axis ct) in
    let impulse = impulse -. jac *. (V.dot axis (V.sub va vb)) in
    max impulse (Constraint.lower_limit ct) |> min (Constraint.upper_limit ct) in

  let update_solver impulse solver ct r =
    let scale = impulse *. solver.SolverBody.mass_inv in
    let inertia = M3.ratio solver.SolverBody.inertia_inv impulse in
    let cross = V.cross r (Constraint.axis ct) in
    {solver with SolverBody.delta_linear_velocity =
        V.scale ~v:(Constraint.axis ct) ~scale;
      delta_angular_velocity = M3.mult_vec ~mat:inertia ~vec:cross;
    } in

  let new_constraint ct fric =
    Constraint.make ~axis:(Constraint.axis ct) ~inv:(Constraint.jac_diag_inv ct)
      ~rhs:(Constraint.rhs ct) ~lower:(-.fric) ~upper:fric ~accum:(Constraint.accum_impulse ct) in

  let cps = Contact.contact_points contact in

  (* TODO 指定回数だけ計算して、拘束力を更新する *)
  let rec solve_onestep solverA solverB step =
    let module S = SolverBody in 
    let ctraint = (ContactPoint.constraints contact).(0) in
    let delta_impulse = Constraint.rhs ctraint in
    let delta_velocity_a = V.add solverA.S.delta_linear_velocity (V.cross solverA.S.delta_angular_velocity rA)
    and delta_velocity_b = V.add solverB.S.delta_linear_velocity (V.cross solverB.S.delta_angular_velocity rB) in
    let delta_impulse = calc_delta delta_impulse ctraint delta_velocity_a delta_velocity_b in
    let solverA = update_solver delta_impulse solverA ctraint rA
    and solverB = update_solver delta_impulse solverB ctraint rB in
    let max_friction = (Contact.friction contact) *. (Constraint.accum_impulse ctraint |> abs_float) in
    let ctraint1 = (ContactPoint.constraints contact).(1)
    and ctraint2 = (ContactPoint.constraints contact).(2) in
    (ContactPoint.constraints contact).(1) <- new_constraint ctraint1 max_friction;
    (ContactPoint.constraints contact).(2) <- new_constraint ctraint2 max_friction;
    let open Engine_option in
    if step >= opt.iteration then (solverA, solverB) else solve_onestep solverA solverB (succ step) in
  let (solverA, solverB) = solver_onestep solverA solverB 0 in
  ((bodyA, solverA), (bodyB, solverB))
;;
