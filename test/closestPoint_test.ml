module C = Realcamel.ClosestPoint
module V = Candyvec.Std.Vector
module Mat = Candyvec.Std.Matrix4
module Q = Candyvec.Std.Quaternion
module M = Realcamel.Mesh
module Voronoi = Realcamel.Voronoi
module RI = Realcamel.RigidBodyInfo
module R = Realcamel.RigidBody
module CO = Realcamel.Collidable
module S = Realcamel.State
module SP = Realcamel.Shape
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

let test_observe_face () =
  let plane = {M.Facet.vertex_ids = (0,0,0);
                edge_ids = (0, 0, 0);
                normal = V.normal_axis `X;
              }
  and normal = V.invert (V.normal_axis `X) in
  assert_equal false (C.Base.is_observe_face plane normal);
  assert_equal true (C.Base.is_observe_face plane (V.normal_axis `X));
  assert_equal true (C.Base.is_observe_face plane {V.x = 0.5;y = 0.5; z = 0.0})

let mesh = M.convert ~vertices ~faces

let make_body pos orientation =
  let collidable = CO.build [|{SP.mesh = mesh;offset_pos = V.zero ;
                              offset_orientation = Q.identity}
                            |]
  in
  let state = {S.empty with S.pos = pos; orientation;} in
  {RI.body = R.empty;state ; collidable}

let test_get_closest_points_on_edges () =
  let body_a = make_body {V.x = 1.0;y = 0.0; z = 0.0} Q.identity in
  let body_b = make_body V.zero Q.identity in

  let points = C.Base.get_edge_closest_points (V.zero, 0.0) body_a body_b (Mat.identity ()) in
  assert_equal 1 (List.length points)

let suite = "detect closest point between meshs" >::: [
  "check the plane to be directed to equal direction for normal" >:: test_observe_face;
  "calculate closest points on edges" >:: test_get_closest_points_on_edges;
]
