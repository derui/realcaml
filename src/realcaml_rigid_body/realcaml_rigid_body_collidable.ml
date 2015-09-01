open Core.Std
module Util = Realcaml_util
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module V = Typedvec.Std.Algebra.Vec

type t = {
  shapes:Shape.t array;
  center:Types.vec;
  half_size:Types.vec;
}

let empty = {
  shapes = [||];
  center = Util.Vec.empty;
  half_size = Util.Vec.empty;
}

let compare_vec comparator v1 v2 =
  let newv = V.copy v1 in
  let compared v1 v2 = Option.map2 v1 v2 ~f:comparator |> Option.value ~default:0.0 in
  V.set ~index:0 ~v:(compared (V.get ~index:0 v1) (V.get ~index:0 v2)) newv;
  V.set ~index:1 ~v:(compared (V.get ~index:1 v1) (V.get ~index:1 v2)) newv;
  V.set ~index:2 ~v:(compared (V.get ~index:2 v1) (V.get ~index:2 v2)) newv;
  newv

let greater_float f1 f2 = if f1 > f2 then f1 else f2
let lesser_float f1 f2 = if f1 < f2 then f1 else f2

let get_transformed_vertices shape =
  let module M = Realcaml_mesh.Mesh in
  let module V = Util.Vec in
  let trans = Shape.offset_transform shape in
  Array.map shape.Shape.mesh.M.vertices ~f:(fun v -> A.mul_v2m (V.to_four v) trans) |>
      Array.map ~f:V.to_three |> Array.to_list

let build shapes =
  let vertices = Array.map ~f:get_transformed_vertices shapes |> Array.to_list |> List.concat in
  let max_point = V.make S.three Float.min_value
  and min_point = V.make S.three Float.max_value in
  let max_point,min_point = List.fold_left vertices ~f:(fun (max_point, min_point) v ->
    let max_point = compare_vec max v max_point
    and min_point = compare_vec min v min_point in
    (max_point, min_point)
  ) ~init:(max_point, min_point)
  in

  let open V.Open in
  let center = V.scalar ~scale:0.5 ~v:(min_point +: max_point) in
  let diagonal = min_point -: max_point in
  let x = (V.get ~index:0 diagonal |> Option.value ~default:0.0) *. 0.5
  and y = (V.get ~index:1 diagonal |> Option.value ~default:0.0) *. 0.5
  and z = (V.get ~index:2 diagonal |> Option.value ~default:0.0) *. 0.5 in

  let half_size = Util.Vec.empty in
  V.set ~index:0 ~v:x half_size;
  V.set ~index:1 ~v:y half_size;
  V.set ~index:2 ~v:z half_size;
  {shapes; center; half_size;}

let rebuild collidable = build collidable.shapes

