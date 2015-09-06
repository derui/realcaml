open Core.Std
type motion = Active | Static

module V = Typedvec.Std.Algebra.Vec
module M = Typedvec.Std.Algebra.Mat
module S = Typedvec.Std.Size
module Q = Typedvec.Std.Ext.Qua
module U = Realcaml_util

type t = {
  pos:U.vec;
  orientation:Q.t;
  linear_velocity:U.vec;
  angular_velocity:U.vec;
  motion_type:motion;
}

let empty =
  {pos = U.Vec.empty (); orientation = Q.identity;
   linear_velocity = U.Vec.empty ();
   angular_velocity = U.Vec.empty ();
   motion_type = Static}

let to_world_transform {pos;orientation;_} = U.to_world_transform pos orientation

let is_static {motion_type;_} =
  match motion_type with
  | Static -> true
  | _ -> false

let is_active {motion_type;_} =
  match motion_type with
  | Active -> true
  | _ -> false
