
module Rigid_body = Realcaml_rigid_body_rigid_body
module Collidable = Realcaml_rigid_body_collidable
module State = Realcaml_rigid_body_state

type t = {
  body:Rigid_body.t;
  collidable:Collidable.t;
  state:State.t;
}

let get_world_transform {state;_} = State.to_world_transform state

let pos rbi = rbi.state.State.pos
let set_pos rbi ~pos = {rbi with state = {rbi.state with State.pos = pos}}
