module C = Realcamel.ClosestPoint
module V = Candyvec.Std.Vector
module M = Realcamel.Mesh
module Voronoi = Realcamel.Voronoi
module RI = Realcamel.RigidBodyInfo
module CO = Realcamel.Collidable
module S = Realcamel.State
open Sugarpot.Std.Math
open Sugarpot.Std
open OUnit

(* triangular pyramid on the x-z plane. *)
let vertices =
  let open Candyvec.Vector in
  [| {x = 0.0;y = 0.0;z = -1.0;};
     {x = -1.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 0.0;z = 0.0;};
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

let suite = "detect closest point between meshs" >::: [
  "check the plane to be directed to equal direction for normal" >:: test_observe_face;
]
