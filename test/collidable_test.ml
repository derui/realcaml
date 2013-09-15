module V = Candyvec.Std.Vector
module Q = Candyvec.Std.Quaternion
module M = Realcamel.Mesh
module C = Realcamel.Collidable
module S = Realcamel.Shape
open Sugarpot.Std.Math
open Sugarpot.Std
open OUnit

(* triangular pyramid on the x-z plane. *)
let vertices =
  let open Candyvec.Vector in
  [| {x = 0.0;y = 0.0;z = -1.0;};
     {x = -0.5;y = 0.0;z = 0.0;};
     {x = 0.5;y = 0.0;z = 0.0;};
     {x = 0.0;y = 1.0;z = -0.5;};
  |]

let faces =
  [| (0, 2, 1);
     (3, 2, 0);
     (3, 0, 1);
     (3, 1, 2);
  |]

let mesh = M.convert ~vertices ~faces

let test_build_collidable () =
  let shape = {S.mesh = mesh; offset_pos = V.zero;
               offset_orientation = Q.identity}
  in
  let collidable = C.build [|shape|] in
  let center = collidable.C.center in
  let printer = Printf.sprintf "%f" in
  begin
    assert_equal ~printer ~msg:"center" 0.0 center.V.x;
    assert_equal ~printer ~msg:"center" 0.5 center.V.y;
    assert_equal ~printer ~msg:"center" (-0.5) center.V.z;
  end;
  let half_size = collidable.C.half_size in
  begin
    assert_equal ~printer ~msg:"half size" 0.5 half_size.V.x;
    assert_equal ~printer ~msg:"half size" 0.5 half_size.V.y;
    assert_equal ~printer ~msg:"half size" 0.5 half_size.V.z;
  end

let suite = "make to be intersect is based on AABB" >::: [
  "can build AABB fitting given meshes" >:: test_build_collidable;
]
