open Sugarpot.Std.Prelude
module V = Candyvec.Std.Vector
module RI = RigidBodyInfo
module M = Candyvec.Std.Matrix4
module MU = Candyvec.Std.Matrix

type t = {
  contact_num:int;
  friction:float;
  contact_points:ContactPoint.t list;
}

let empty = {contact_num = 0; friction = 0.0; contact_points = []}

let calc_next_contact contact next_list =
  if List.length next_list <= 4 then
    {contact with contact_points = next_list; contact_num = List.length next_list }
  else
      (* TRANSLATE:限度の個数以上になる場合、面積が最大になるような衝突点のみ選択する  *)
    let sorted_list = List.sort (fun cp1 cp2 ->
        compare cp1.ContactPoint.distance cp2.ContactPoint.distance) next_list |> List.rev in
    let max_dist = List.hd sorted_list in
    let points_without_max = Array.of_list |< List.tl sorted_list in
    let get_combi (a,b,c) = (points_without_max.(a),
                             points_without_max.(b),
                             points_without_max.(c)) in
    let combi_points = [get_combi (0, 1, 2);
                        get_combi (0, 2, 3);
                        get_combi (0, 1, 3);
                        get_combi (1, 2, 3);
                       ] in
    let calc_space a b c d =
      let a = a.ContactPoint.pointA
      and b = b.ContactPoint.pointA
      and c = c.ContactPoint.pointA
      and d = d.ContactPoint.pointA in
      List.sort compare [V.cross (V.sub a c) (V.sub b d) |> V.norm |> abs_float;
                         V.cross (V.sub a b) (V.sub d c) |> V.norm |> abs_float;
                         V.cross (V.sub a d) (V.sub b c) |> V.norm |> abs_float;] |> List.hd in
      (* TRANSLATE: 最大のdistを中心として、残りを組み合わせてチェックする *)
    let ((a, b, c, d), _) =
      (List.map (fun (b, c, d) -> ((max_dist,b,c,d), calc_space max_dist b c d)) combi_points) |>
          List.sort (fun (_, square1) (_, square2) -> compare square1 square2) |> List.hd in
    {contact with contact_num = 4; contact_points = [a;b;c;d]}

let update_contact_points ~contact ~body_a ~body_b ~closest =
  (* TRANSLATE: 最近接点をpairに追加する。 *)
  let point = closest in
  let open Candyvec.Std.Matrix4.Open in
  let world_a = RI.get_world_transform body_a in
  let world_b = world_a *|> (RI.get_world_transform body_b |> MU.force_inverse) in
  Printf.printf "depth %f \n" closest.ClosestPoint.depth;
  Printf.printf "point A %s \n" (V.to_string closest.ClosestPoint.point_a);
  Printf.printf "point B %s \n" (V.to_string closest.ClosestPoint.point_b);
  Printf.printf "normal %s \n" (V.to_string closest.ClosestPoint.normal);

  let new_point = {ContactPoint.empty with ContactPoint.distance = closest.ClosestPoint.depth;
                   pointA = point.ClosestPoint.point_a; pointB = point.ClosestPoint.point_b *||> world_b;
                   normal = closest.ClosestPoint.normal ;
                  } in
  let points_list = new_point :: contact.contact_points in
  calc_next_contact contact points_list
