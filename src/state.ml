type motion = Actice | Static

type t = {
  pos:Vecmath.Vector.t;
  orientation:Vecmath.Quaternion.t;
  linear_velocity:Vecmath.Vector.t;
  angular_velocity:Vecmath.Vector.t;
  motion_type:motion;
}

let make ~pos ~orient ~linear ~angular ~motion_type =
  {pos; orientation = orient; linear_velocity = linear;
   angular_velocity = angular; motion_type;}

let empty =
  {pos = Vecmath.Vector.zero ; orientation = Vecmath.Quaternion.identity ();
   linear_velocity = Vecmath.Vector.zero;
   angular_velocity = Vecmath.Vector.zero;
   motion_type = Static}

let pos {pos;_} = pos

let orientation {orientation;_} = orientation

let linear_velocity {linear_velocity;_} = linear_velocity

let angular_velocity {angular_velocity;_} = angular_velocity

let motion_type {motion_type;_} = motion_type
