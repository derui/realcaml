type motion = Active | Static

module V = Candyvec.Std.Vector
module Q = Candyvec.Std.Quaternion

type t = {
  pos:V.t;
  orientation:Q.t;
  linear_velocity:V.t;
  angular_velocity:V.t;
  motion_type:motion;
}

let empty =
  {pos = V.zero ; orientation = Q.identity;
   linear_velocity = V.zero;
   angular_velocity = V.zero;
   motion_type = Static}

let to_world_transform {pos;orientation;_} =
  Util.world_transform orientation pos

let is_static s =
  match s.motion_type with
  | Static -> true
  | _ -> false

let is_active s =
  match s.motion_type with
  | Active -> true
  | _ -> false
