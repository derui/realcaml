module V = Candyvec.Vector

let apply_force ~body ~state ~force ~torque ~time_step =
  (* TRANSLATE: Forceを計算する *)
  let delta_accel = V.scale ~v:force ~scale:(1.0 /. body.RigidBody.mass) in
  let delta_velocity = V.scale ~v:delta_accel ~scale:time_step in
  (* TRANSLATE: オイラー陽解法を利用する *)
  let velocity = state.State.linear_velocity in
  let new_velocity = V.add velocity delta_velocity in
  (body, {state with State.linear_velocity = new_velocity})
