module Rigid_body = Realcaml_rigid_body.Rigid_body
module State = Realcaml_rigid_body.State
module V = Typedvec.Algebra.Vec

let apply_force ~body ~state ~force ~torque:_ ~time_step () =
  (* TRANSLATE: Forceを計算する *)
  let open V.Open in
  let delta_accel = V.scalar ~scale:(1.0 /. body.Rigid_body.mass) force in
  let delta_velocity = V.scalar ~scale:time_step delta_accel in
  (* TRANSLATE: オイラー陽解法を利用する *)
  let velocity = state.State.linear_velocity in
  let new_velocity = velocity +: delta_velocity in
  (body, { state with State.linear_velocity = new_velocity })
