open Candyvec

type t = {
  body:RigidBody.t;
  collidable:Collidable.t;
  state:State.t;
}

let get_world_transform {state;_} =
  let pos = state.State.pos  
  and orientation = state.State.orientation in
  let trans_mat = Matrix4.translation pos
  and orient_mat = Quaternion.to_matrix orientation in
  Matrix4.multiply trans_mat orient_mat

let set_pos rbi pos =
  {rbi with state = {rbi.state with State.pos = pos}}
