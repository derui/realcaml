open Vecmath

type t = {
  body:RigidBody.t;
  collidable:Collidable.t;
  state:State.t;
}

let make ~body ~col ~state =
  {body; collidable = col; state;}

let rigid_body {body;_} = body

let collidable {collidable;_} = collidable

let state {state;_} = state

let get_world_transform {state;_} =
  let pos = State.pos state
  and orientation = State.orientation state in
  let trans_mat = Matrix4.translation pos
  and orient_mat = Quaternion.to_matrix orientation in
  Matrix4.multiply trans_mat orient_mat
