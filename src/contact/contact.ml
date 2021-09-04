open Core
module A = Typedvec.Algebra
module V = Typedvec.Algebra.Vec
module RI = Realcaml_rigid_body
module M = Typedvec.Algebra.Mat
module U = Realcaml_util

type t = {
  contact_num : int;
  friction : float;
  contact_points : Contact_point.t list;
}

let empty = { contact_num = 0; friction = 0.0; contact_points = [] }

let calc_next_contact contact next_list =
  if List.length next_list <= 4 then { contact with contact_points = next_list; contact_num = List.length next_list }
  else
    (* TRANSLATE:限度の個数以上になる場合、面積が最大になるような衝突点のみ選択する *)
    let sorted_list =
      List.sort next_list ~compare:(fun cp1 cp2 -> Stdlib.compare cp1.Contact_point.distance cp2.Contact_point.distance)
      |> List.rev
    in
    let max_dist = List.hd_exn sorted_list in
    let points_without_max = List.tl sorted_list |> Option.value ~default:[] |> Array.of_list in
    let get_combi (a, b, c) = (points_without_max.(a), points_without_max.(b), points_without_max.(c)) in
    let combi_points = [ get_combi (0, 1, 2); get_combi (0, 2, 3); get_combi (0, 1, 3); get_combi (1, 2, 3) ] in
    let calc_space a b c d =
      let a = a.Contact_point.pointA
      and b = b.Contact_point.pointA
      and c = c.Contact_point.pointA
      and d = d.Contact_point.pointA in
      let open V.Open in
      List.sort ~compare:Stdlib.compare
        [
          V.cross ~left:(a -: c) ~right:(b -: d) |> V.norm |> Float.abs;
          V.cross ~left:(a -: b) ~right:(d -: c) |> V.norm |> Float.abs;
          V.cross ~left:(a -: d) ~right:(b -: c) |> V.norm |> Float.abs;
        ]
      |> List.hd
    in
    (* TRANSLATE: 最大のdistを中心として、残りを組み合わせてチェックする *)
    let (a, b, c, d), _ =
      List.map combi_points ~f:(fun (b, c, d) -> ((max_dist, b, c, d), calc_space max_dist b c d))
      |> List.sort ~compare:(fun (_, square1) (_, square2) -> Stdlib.compare square1 square2)
      |> List.hd_exn
    in
    { contact with contact_num = 4; contact_points = [ a; b; c; d ] }

let update_contact_points ~body_a ~body_b ~closest contact =
  (* TRANSLATE: 最近接点をpairに追加する。 *)
  let module RBI = RI.Rigid_body_info in
  let open M.Open in
  let point = closest in
  let world_a = RBI.get_world_transform body_a in
  let world_b_inv =
    match RBI.get_world_transform body_b |> M.inverse with
    | Some m -> m
    | None   -> failwith "No have inverse matrix of world matrix"
  in
  let world_b = world_a *: world_b_inv in

  let open A.Open in
  let new_point = Contact_point.empty () in
  let new_point =
    {
      new_point with
      Contact_point.distance = closest.Closest_point.depth;
      pointA = point.Closest_point.point_a;
      pointB =
        (let v = U.Vec.to_four point.Closest_point.point_b in
         v *> world_b |> U.Vec.to_three);
      normal = closest.Closest_point.normal;
    }
  in
  let points_list = new_point :: contact.contact_points in
  calc_next_contact contact points_list
