open Core.Std

module A = Typedvec.Std.Algebra
module V = A.Vec
module U = Realcaml_util

type v = U.vec

module Region = struct
  (* TRANSLATE: 半時計回りの順序を持つ頂点と、法線ベクトル *)
  type t = {
    triangle: v * v * v;
    normal: v;
  }

  let is_contain edge point = let open V.Open in point *: edge >= 0.0
  let recent_point ~point t = 
    let open V.Open in
    let {triangle = (p1, p2, p3); normal} = t in
    let subbed = normal *: (point -: p1) in
    let multiplied = V.scalar ~scale:subbed normal in
    let projected = point -: multiplied in
    (* 各エッジに対する、面の法線ベクトルとの外積を、エッジの法線ベクトルとして使う *)
    let edges = [p2 -: p1; p3 -: p2; p1 -: p3] in
    let normals = List.map edges ~f:(fun e -> V.cross e normal |> V.normalize) in
    (* すべてのエッジの法線ベクトルの裏側に点がある場合、内部に存在すると判定できる *)
    let is_contain = List.for_all normals (fun n -> projected *: n < 0.0) in
    if is_contain then Some projected else None
end

(* TRANSLATE: 面の領域を求める *)
let make (p1, p2, p3) =
  let open V.Open in
  let e2 = p3 -: p2
  and e3 = p1 -: p3 in
  {Region.triangle = (p1, p2, p3); normal = V.cross e2 e3}
