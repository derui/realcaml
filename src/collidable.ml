type t = {
  shapes:Shape.t array;
  center:Vecmath.Vector.t;
  half_size:Vecmath.Vector.t;
}

let empty = {shapes = [||];
             center = Vecmath.Vector.zero;
             half_size = Vecmath.Vector.zero
            }
