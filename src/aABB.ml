(** A `AABB` type.  *)
type t = {center:Vecmath.Vector.t; half_size: Vecmath.Vector.t}

let make ~center ~half_size = {center;half_size}

let intersect_one_axis ~pos_a ~len_a ~pos_b ~len_b =
  let a_min = pos_a -. len_a
  and a_max = pos_a +. len_a
  and b_min = pos_b -. len_b
  and b_max = pos_b +. len_b in
  if a_max < b_min then false
  else if a_min > b_max then false
  else true

let intersect box_a box_b =
  let open Vecmath.Vector in
  let x_sect = intersect_one_axis box_a.center.x box_b.half_size.x
    box_b.center.x box_b.half_size.x
  and y_sect = intersect_one_axis box_a.center.y box_b.half_size.y
    box_b.center.y box_b.half_size.y
  and z_sect = intersect_one_axis box_a.center.z box_b.half_size.z
    box_b.center.z box_b.half_size.z in
  x_sect && y_sect && z_sect
