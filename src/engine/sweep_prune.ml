module Rigid_body = Realcaml_rigid_body
module A = Typedvec.Algebra
module RBI = Rigid_body.Rigid_body_info
module M = A.Mat
module Q = Typedvec.Ext.Qua
module U = Realcaml_util
module AABB = Realcaml_rigid_body.AABB

type t = {
  bodies : RBI.t option array;
  current_count : int;
}

type intersect = RBI.t * RBI.t

let make count = { current_count = 0; bodies = Array.make count None }

let add prune info =
  let current = prune.current_count in
  prune.bodies.(current) <- Some info;
  { prune with current_count = succ current }

let vec_to_translated v world =
  let open A.Open in
  let open M.Open in
  let module AF = Typedvec.Ext.Affine in
  let module V = A.Vec in
  let af = AF.make Typedvec.Size.three in
  let trans = (AF.translate af ~vec:v |> AF.to_mat) *: world in
  U.Vec.four_empty () *> trans |> U.Vec.to_three

let intersect info ind_a ind_b =
  let module C = Rigid_body.Collidable in
  let in_range i range = i >= 0 && i < range in
  if (not (in_range ind_a info.current_count)) || not (in_range ind_b info.current_count) then None
  else
    match (info.bodies.(ind_a), info.bodies.(ind_b)) with
    | None, _ | _, None      -> None
    | Some a_inf, Some b_inf ->
        let a_trans = RBI.get_world_transform a_inf and b_trans = RBI.get_world_transform b_inf in
        let a_box = a_inf.RBI.collidable.C.aabb and b_box = b_inf.RBI.collidable.C.aabb in

        let a_pos = vec_to_translated a_box.AABB.center a_trans
        and b_pos = vec_to_translated b_box.AABB.center b_trans in

        let a_box = AABB.make ~center:a_pos ~half_size:a_box.AABB.half_size ()
        and b_box = AABB.make ~center:b_pos ~half_size:b_box.AABB.half_size () in

        if AABB.intersect a_box b_box then Some (a_inf, b_inf) else None
