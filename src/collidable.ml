type t = {
  shapes:Shape.t array;
  center:Candyvec.Vector.t;
  half_size:Candyvec.Vector.t;
}

let empty = {shapes = [||];
             center = Candyvec.Vector.zero;
             half_size = Candyvec.Vector.zero
            }
