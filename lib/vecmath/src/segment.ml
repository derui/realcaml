module V = Vector

(** Type of segment. *)
type t = Vector.t * Vector.t

let make ~start ~last = (start,  V.sub last start)

let closest_with_point (start, slope) point =
  let d = V.dot (V.sub point start) (V.normalize slope) in
  let s = d /. (V.norm slope) in
  if s >= 0.0 && s <= 1.0 then V.add start (V.scale ~v:slope ~scale:s)
  else if s < 0.0 then start
  else V.add start slope


module Mat2D = struct
  (* m12 = row at 1 and column at 2 *)
  type t = { m11 : float; m12: float; m21:float; m22:float}

  let make = { m11 = 1.0; m12 = 0.0; m21 = 0.0; m22 = 1.0}
  let determinant mat = mat.m11 *. mat.m22 -. mat.m12 *. mat.m21 = 0.0

  let transpose mat = {mat with m12 = mat.m21; m21 = mat.m12}
  let inverse mat =
    if determinant mat then
      let adj = { m11 = mat.m22; m12 = -1.0 *. mat.m21; m21 = -1.0 *. mat.m12; m22 = mat.m11} in
      Some(transpose adj)
    else None

  let multiply mat (v1, v2) =
    (mat.m11 *. v1 +. mat.m12 *. v2, mat.m21 *. v1 +. mat.m22 *. v2)
end

let lines_intersect_points (start1, slope1) (start2, slope2) =
  let c = V.sub start1 start2 in
  let mat = {Mat2D.m11 = (V.dot slope1 slope1);
             m12 = (-1.0 *. (V.dot slope2 slope2));
             m21 = (-1.0 *. (V.dot slope1 slope2));
             m22 = (V.dot slope2 slope2)} in
  match Mat2D.inverse mat with
  | Some(mat) ->
    Mat2D.multiply mat (-1.0 *. (V.dot slope1 c), -1.0 *. (V.dot slope2 c))
  | None -> (1.0, 1.0)

let closest s1 s2 =
  (* TRANSLATE: s1を含む直線上における、仮の最近接点を取得する *)
  let (s, t) = lines_intersect_points s1 s2 in
  (* ここで、s > 1.0の場合、s = 1.0として仮の点とする。 *)
  let s = if s > 1.0 then 1.0 else s in
  let trial_e1 = V.scale ~v:(fst s1) ~scale:s in
  let trial_e2 = closest_with_point s2 trial_e1 in
  (closest_with_point s1 trial_e1, trial_e2)
