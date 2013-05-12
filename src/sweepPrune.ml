open Baselib.Std.Prelude
module RBI = RigidBodyInfo

type t = {
  infos:RBI.t option array;
  current_count: int;
}

let make count =
  {current_count = 0; infos = Array.make count None;}

let add prune info =
  let current = prune.current_count in
  prune.infos.(current) <- Some info;
  {prune with current_count = succ current}
  
let intersect info ind_a ind_b =
  let in_range i range = i > 0 && i < range in
  if not (in_range ind_a info.current_count) ||
    not (in_range ind_b info.current_count) then None
  else
    match (info.infos.(ind_a), info.infos.(ind_b)) with
    | (None, _) | (_, None) -> None
    | (Some a_inf, Some b_inf) ->
      let a_pos = a_inf.RBI.collidable.Collidable.center
      and a_size = a_inf.RBI.collidable.Collidable.half_size
      and b_pos = b_inf.RBI.collidable.Collidable.center
      and b_size = b_inf.RBI.collidable.Collidable.half_size in
      let open Vecmath.Vector in
      (* TRANSLATE: X軸から衝突をチェックしてみる *)
      if AABB.intersect_one_axis ~pos_a:a_pos.x ~len_a:a_size.x
        ~pos_b:b_pos.x ~len_b:b_size.x then
      (* TRANSLATE: X軸で交差していた場合、残り二軸についても衝突判定する。 *)
        let iy = AABB.intersect_one_axis ~pos_a:a_pos.y ~len_a:a_size.y
          ~pos_b:b_pos.y ~len_b:b_size.y
        and iz = AABB.intersect_one_axis ~pos_a:a_pos.z ~len_a:a_size.z
          ~pos_b:b_pos.z ~len_b:b_size.z in
      (* TRANSLATE: 3軸全てが交差している場合は衝突していると判定する。 *)
        if iy && iz then Some (a_inf, b_inf) else None
      else
        None
