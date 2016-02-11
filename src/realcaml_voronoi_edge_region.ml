open Core.Std

module A = Typedvec.Std.Algebra
module V = A.Vec
module U = Realcaml_util

type v = U.vec

module Region = struct
  (* TRANSLATE: エッジの始点と終点、エッジの法線ベクトル、面の法線ベクトル *)
  type t = {
    edge: v * v;
    normal: v;
    face_normal: v;
  }

  let is_contain edge point = let open V.Open in point *: edge >= 0.0

  (* translate: エッジのボロノイ領域に含まれるかどうかを返却する *)
  let recent_point ~point t = 
    let {edge = (e1, e2); normal = enormal; _} = t in
    let open V.Open in
    let edge1 = V.normalize (e2 -: e1) in
    let edge2 = V.inverse edge1
    and base1 = point -: e1
    and base2 = point -: e2 in
    let contained = List.for_all ~f:ident [
      is_contain edge1 base1; is_contain edge2 base2; is_contain enormal point
    ] in
    if not contained then None
    else
      let edge = e2 -: e1 in
      Some (V.scalar edge ~scale:(edge *: (point -: e1)))
end

(* TRANSLATE : エッジと面の法線から、エッジのボロノイ領域を取得する *)
let make (v1, v2) normal =
  let open V.Open in
  let edge_normal = V.cross (v2 -: v1) normal |> V.normalize in
  {Region.edge = (v1, v2); normal = edge_normal;face_normal=normal}

