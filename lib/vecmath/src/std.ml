module Matrix3 = Matrix3
module Matrix4 = Matrix4
module Quaternion = Quaternion
module Segment = Segment

(* define extended vector *)
module Vector = struct
  include Vector

  include Vector_ext
end
