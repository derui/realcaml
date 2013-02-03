type t = {
  shapes:Shape.t array;
  center:Vecmath.Vector.t;
  half_size:Vecmath.Vector.t;
}

let make ~shapes ~center ~half_size = {shapes;center;half_size}

let empty = {shapes = [||];
             center = Vecmath.Vector.zero;
             half_size = Vecmath.Vector.zero
            }

let center {center;_} = center

let half_size {half_size;_} = half_size

let shapes {shapes;_} = shapes
