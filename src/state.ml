type motion = Active | Static

open Candyvec.Std

type t = {
  pos:Vector.t;
  orientation:Quaternion.t;
  linear_velocity:Vector.t;
  angular_velocity:Vector.t;
  motion_type:motion;
}

let empty =
  {pos = Vector.zero ; orientation = Quaternion.identity ();
   linear_velocity = Vector.zero;
   angular_velocity = Vector.zero;
   motion_type = Static}
;;

let to_world_transform {pos;orientation;_} =
  Util.world_transform orientation pos
;;
