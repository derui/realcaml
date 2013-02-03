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
