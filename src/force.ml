module V = Candyvec.Vector

let apply_force ~body ~state ~force ~torque ~time_step =
  (* TRANSLATE: 撃力を計算する *)
  let impulse = V.scale ~v:force ~scale:time_step in
(* translate: オイラー陽解法を利用する *)
  let velocity = state.State.linear_velocity
  and pos = state.State.pos in
  let new_velocity = V.sub velocity (V.scale ~v:pos ~scale:time_step) in
  let new_pos = V.add pos (V.scale ~v:velocity ~scale:time_step) in
  let new_velocity = V.add new_velocity impulse in
  (body, {state with State.pos = new_pos; linear_velocity = new_velocity})
