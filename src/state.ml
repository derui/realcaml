type motion = Active | Static

type t = {
  pos:Candyvec.Vector.t;
  orientation:Candyvec.Quaternion.t;
  linear_velocity:Candyvec.Vector.t;
  angular_velocity:Candyvec.Vector.t;
  motion_type:motion;
}

let empty =
  {pos = Candyvec.Vector.zero ; orientation = Candyvec.Quaternion.identity ();
   linear_velocity = Candyvec.Vector.zero;
   angular_velocity = Candyvec.Vector.zero;
   motion_type = Static}
