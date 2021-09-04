type motion =
  | Active
  | Static

module V = Typedvec.Algebra.Vec
module M = Typedvec.Algebra.Mat
module S = Typedvec.Size
module Q = Typedvec.Ext.Qua
module U = Realcaml_util

type t = {
  pos : U.Types.vec;
  orientation : Q.t;
  linear_velocity : U.Types.vec;
  angular_velocity : U.Types.vec;
  motion_type : motion;
}

let empty =
  {
    pos = U.Vec.empty ();
    orientation = Q.identity;
    linear_velocity = U.Vec.empty ();
    angular_velocity = U.Vec.empty ();
    motion_type = Static;
  }

let to_world_transform { pos; orientation; _ } = U.Fn.to_world_transform pos orientation

let is_static { motion_type; _ } = match motion_type with Static -> true | _ -> false

let is_active { motion_type; _ } = match motion_type with Active -> true | _ -> false
