type motion = Active | Static

type t = {
  pos:Vecmath.Vector.t;
  orientation:Vecmath.Quaternion.t;
  linear_velocity:Vecmath.Vector.t;
  angular_velocity:Vecmath.Vector.t;
  motion_type:motion;
}

let empty =
  {pos = Vecmath.Vector.zero ; orientation = Vecmath.Quaternion.identity ();
   linear_velocity = Vecmath.Vector.zero;
   angular_velocity = Vecmath.Vector.zero;
   motion_type = Static}
