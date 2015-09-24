(** A `AABB` type.  *)
module A = Typedvec.Std.Algebra
module V = A.Vec

type t = {
  center: Realcaml_util_types.vec;
  half_size: Realcaml_util_types.vec;
}

let intersect_one_axis ~pos_a ~len_a ~pos_b ~len_b () =
  let a_min = pos_a -. len_a
  and a_max = pos_a +. len_a
  and b_min = pos_b -. len_b
  and b_max = pos_b +. len_b in
  if a_max < b_min then false
  else if a_min > b_max then false
  else true

let intersect box_a box_b =
  (* ある軸に対してintersectしているかどうかを調べる*)
  let intersect_axis index =
    let a_p = V.unsafe_get box_a.center index
    and a_len = V.unsafe_get box_a.half_size index
    and b_p = V.unsafe_get box_b.center index
    and b_len = V.unsafe_get box_b.half_size index in
    intersect_one_axis a_p a_len b_p b_len ()
  in
  let x_sect = intersect_axis 0
  and y_sect = intersect_axis 1
  and z_sect = intersect_axis 2 in
  x_sect && y_sect && z_sect
