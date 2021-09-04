open Core
module A = Typedvec.Algebra
module V = A.Vec
module U = Realcaml_util

type t = {
  center : U.Types.vec;
  half_size : U.Types.vec;
}
(** A `AABB` type. *)

let empty () = { center = U.Vec.empty (); half_size = U.Vec.empty () }

let make ~center ~half_size () = { center; half_size }

let intersect_one_axis ~pos_a ~len_a ~pos_b ~len_b =
  let a_min = pos_a -. len_a and a_max = pos_a +. len_a and b_min = pos_b -. len_b and b_max = pos_b +. len_b in
  let open Float in
  if a_max < b_min then false else if a_min > b_max then false else true

let intersect box_a box_b =
  (* ある軸に対してintersectしているかどうかを調べる*)
  let intersect_axis index =
    let pos_a = V.unsafe_get box_a.center index
    and len_a = V.unsafe_get box_a.half_size index
    and pos_b = V.unsafe_get box_b.center index
    and len_b = V.unsafe_get box_b.half_size index in
    intersect_one_axis ~pos_a ~len_a ~pos_b ~len_b
  in
  let open Lazy in
  (* それぞれをLazyにすることで、必要最小限のチェックで行える *)
  from_fun (fun () -> intersect_axis 0)
  >>= (function false -> return false | true -> return (intersect_axis 1))
  >>= (function false -> return false | true -> return (intersect_axis 2))
  |> force
