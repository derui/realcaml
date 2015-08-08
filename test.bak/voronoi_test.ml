module Voronoi = Realcamel.Voronoi
open Sugarpot.Std.Math
open OUnit

(* Triangle on the x-z plane. *)
let vertices =
  let open Candyvec.Vector in
  [| {x = 0.0;y = 0.0;z = -1.0;};
     {x = -1.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 0.0;z = 0.0;};
  |]
;;

let faces =
  [| (0, 1, 2);
     (1, 2, 3);
  |]
;;

let triangle =  (vertices.(0), vertices.(1), vertices.(2))
;;

let test_make_region () =
  let regions  = Voronoi.Base.make_region triangle in
  assert_equal 6 (List.length regions);
;;

let test_detect_recent_region () =
  let regions  = Voronoi.Base.make_region triangle in
  let point = let open Candyvec.Vector in
              {x = 2.0;y = 0.0; z = 0.1;} in

  let recent = Voronoi.Base.recent_region ~region:regions ~point in
  match recent with
  | Voronoi.Point v ->
    let open Candyvec.Std.Vector in
    assert_equal v.x 1.0;
    assert_equal v.y 0.0;
    assert_equal v.z 0.0;
  | _ -> assert_failure "no recent point it would be returned"
;;

let test_projection_point () =
  let open Candyvec.Std.Vector in
  let base = {x = 0.0;y = 0.0; z = 0.0}
  and normal = {x = 0.0;y = 1.0; z = 0.0}
  and point = normalize {x = 1.0;y = 1.0; z = 1.0} in
  let projected = Voronoi.Base.projection_point ~base ~normal ~point in
  let expected = {x = point.x; y = 0.0; z = point.z} in
  assert_equal ~printer:(Printf.sprintf "%f") expected.x projected.x;
  assert_equal ~printer:(Printf.sprintf "%f") expected.y projected.y;
  assert_equal ~printer:(Printf.sprintf "%f") expected.z projected.z;
;;

let suite = "making and detect recent point with voronoi region" >::: [
  "making voronoi regions" >:: test_make_region;
  "detect recent of point" >:: test_detect_recent_region;
  "calculate projected point" >:: test_projection_point;
]
