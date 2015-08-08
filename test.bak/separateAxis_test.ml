module S = Realcamel.SeparatingAxis
module V = Candyvec.Std.Vector
open Sugarpot.Std.Math
open Sugarpot.Std
open OUnit

(* Triangle on the x-z plane. *)
let vertices =
  let open Candyvec.Std.Vector in
  [| {x = 0.0;y = 0.0;z = -1.0;};
     {x = -1.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 0.0;z = 0.0;};
  |]

let faces =
  [| (0, 1, 2);
     (1, 2, 3);
  |]

let triangle =  (vertices.(0), vertices.(1), vertices.(2))

let test_getting_maximum () =
  let maximum, minimum = S.Base.get_maximum_range {V.x = 1.0; y = 0.0; z = 0.0}
    [|{V.x = 2.0;y = 0.0; z = 0.0};
      {V.x = 3.0; y = 1.0; z = 0.0};
      {V.x = -1.0;y = 0.0; z = 0.0};
    |]
  in begin
    let printer = Printf.sprintf "%f" in
    assert_equal ~printer 3.0 maximum;
    assert_equal ~printer (-1.0) minimum;
  end

let test_detect_separation () =
  let result = S.Base.detect_separation {V.x = 1.0; y = 0.0; z = 0.0}
    (1.0, 0.5) (0.8, 0.2)
  in begin
    let printer = Printf.sprintf "%f" in
    let cmp f1 f2 = compare_float f1 f2 = 0 in

    assert_equal true (Option.is_some result);
    let axis, dist = Option.get result in
    assert_equal ~printer ~cmp (-0.3) dist;
    assert_equal ~printer ~cmp (1.0) axis.V.x;
  end;

  let result = S.Base.detect_separation {V.x = 1.0; y = 0.0; z = 0.0}
    (0.8, 0.5) (1.0, 0.7)
  in begin
    let printer = Printf.sprintf "%f" in
    let cmp f1 f2 = compare_float f1 f2 = 0 in

    assert_equal true (Option.is_some result);
    let axis, dist = Option.get result in
    assert_equal ~printer ~cmp (-0.1) dist;
    assert_equal ~printer ~cmp (-1.0) axis.V.x;
  end


let suite = "making and detect recent point with voronoi region" >::: [
  "getting projected values on the axis" >:: test_getting_maximum;
  "detect separating axis" >:: test_detect_separation;
]
