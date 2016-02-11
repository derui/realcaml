open Core.Std

module A = Typedvec.Std.Algebra
module V = A.Vec
module U = Realcaml_util

type v = U.vec

module Region = struct
  (* TRANSLATE: 基準となる点と、点から伸びるエッジそれぞれの法線ベクトル、面の法線ベクトル *)
  type t = {
    base: v;
    a_normal: v;
    b_normal: v;
    normal: v;
  }

  let is_contain edge point = let open V.Open in point *: edge >= 0.0
  let recent_point ~point t = 
    let open V.Open in
    let {base = p; a_normal = e1; b_normal = e2; normal} = t in
    let base = point -: p in
    let is_contain edge = is_contain edge base in
    if is_contain e1 && is_contain e2 then Some p else None
end


(* TRANSLATE: 指定された点を含む二本のエッジを取得する *)
let point_of_contained_edge v (e1, e2, e3) =
  let point_contained (e1, e2) v = V.equals v e1 ||  V.equals v e2 in
  let cont_e1 = point_contained e1 v
  and cont_e2 = point_contained e2 v
  and cont_e3 = point_contained e3 v in
  match (cont_e1, cont_e2, cont_e3) with
  | (true, true, _) -> (e1, e2)
  | (_, true, true) -> (e2, e3)
  | (true, _, true) -> (e3, e1)
  | _ -> failwith "matching point of edge not found"

(* TRANSLATE: 点のボロノイ領域を求める *)
let make (e1, e2, e3) normal point =
  let (e11, e12), (e21, e22) = point_of_contained_edge point (e1, e2, e3) in
  let a = V.sub e12 e11 |> V.normalize
  and b = V.sub e21 e22 |> V.normalize in
  {Region.base = point; a_normal = a; b_normal = b; normal}
