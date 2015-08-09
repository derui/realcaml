open Core.Std
type motion = Active | Static

module V = Typedvec.Std.Algebra.Vec
module M = Typedvec.Std.Algebra.Mat
module S = Typedvec.Std.Size
module Q = Typedvec.Std.Ext.Qua

type t = {
  pos:Types.vec;
  orientation:Q.t;
  linear_velocity:Types.vec;
  angular_velocity:Types.vec;
  motion_type:motion;
}

let empty =
  {pos = Util.Vec.empty; orientation = Q.identity;
   linear_velocity = Util.Vec.empty;
   angular_velocity = Util.Vec.empty;
   motion_type = Static}

let to_world_transform {pos;orientation;_} = Util.to_world_transform pos orientation

let is_static {motion_type;_} =
  match motion_type with
  | Static -> true
  | _ -> false

let is_active {motion_type;_} =
  match motion_type with
  | Active -> true
  | _ -> false
