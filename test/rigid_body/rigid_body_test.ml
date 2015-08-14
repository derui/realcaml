open Realcaml_mesh
module R = Realcaml_rigid_body
module V = Typedvec.Algebra.Vec
module M = Typedvec.Algebra.Mat
module S = Typedvec.Size
module Q = Typedvec.Std.Ext.Qua

let to_vert ~x ~y ~z () =
  let v = V.make S.three 0.0 in
  V.set v ~index:0 ~v:x;
  V.set v ~index:1 ~v:y;
  V.set v ~index:2 ~v:z;
  v
  
let vertices =
  [| to_vert ~x:0.0 ~y:1.0 ~z:0.0 ();
     to_vert ~x:0.0 ~y:0.0 ~z:0.0 ();
     to_vert ~x:1.0 ~y:0.0 ~z:0.0 ();
     to_vert ~x:1.0 ~y:1.0 ~z:0.0 ();
  |]

let faces =
  [|
    (0, 1, 2);
    (1, 2, 3);
  |]

let%spec "Shape can make matrix to transform for offset the mesh" =
  let mesh = Mesh.convert ~vertices ~faces in
  let offset_pos = R.Util.Vec.empty in
  let orientation = Q.identity in
  let shape = {R.Shape.mesh;offset_pos;offset_orientation = orientation} in
  let trans = R.Shape.offset_transform shape in
  (M.get ~row:0 ~col:0 trans) [@eq Some(1.0)];
  (M.get ~row:1 ~col:1 trans) [@eq Some(1.0)];
  (M.get ~row:1 ~col:0 trans) [@eq Some(0.0)];
  (M.get ~row:0 ~col:1 trans) [@eq Some(0.0)];
  (M.get ~row:3 ~col:3 trans) [@eq Some(1.0)]


