module V = Candyvec.Std.Vector
open Sugarpot.Std.Prelude

type t = {
  shapes:Shape.t array;
  center:Candyvec.Vector.t;
  half_size:Candyvec.Vector.t;
}

let empty = {shapes = [||];
             center = Candyvec.Vector.zero;
             half_size = Candyvec.Vector.zero
            }

let compare_vec comparator v1 v2 =
  let x = comparator v1.V.x v2.V.x
  and y = comparator v1.V.y v2.V.y
  and z = comparator v1.V.z v2.V.z in
  {V.x = x;y;z}

let greater_float f1 f2 = if f1 > f2 then f1 else f2
let lesser_float f1 f2 = if f1 < f2 then f1 else f2

let get_transformed_vertices shape =
  let trans = Util.world_transform shape.Shape.offset_orientation shape.Shape.offset_pos in
  let transformed = Mesh.transform_vertices shape.Shape.mesh trans in
  Array.to_list transformed.Mesh.vertices
  
let build shapes =
  let vertices = Array.map get_transformed_vertices shapes |> Array.to_list |> List.concat in
  let max_point = {V.x = min_float; y = min_float; z = min_float}
  and min_point = {V.x = max_float; y = max_float; z = max_float} in
  let max_point,min_point = List.fold_left (fun (max_point, min_point) v ->
    let max_point = compare_vec greater_float v max_point
    and min_point = compare_vec lesser_float v min_point in
    (max_point, min_point)
  ) (max_point, min_point) vertices
  in

  let center = V.scale ~scale:0.5 ~v:(V.add min_point max_point) in
  let diagonal = V.sub min_point max_point in
  let half_size_x = (V.dot (V.normal_axis `X) diagonal) *. 0.5
  and half_size_y = (V.dot (V.normal_axis `Y) diagonal) *. 0.5
  and half_size_z = (V.dot (V.normal_axis `Z) diagonal) *. 0.5 in

  let half_size = {V.x = V.norm |< V.scale ~scale:half_size_x ~v:(V.normal_axis `X);
                   y = V.norm |< V.scale ~scale:half_size_y ~v:(V.normal_axis `Y);
                   z = V.norm |< V.scale ~scale:half_size_z ~v:(V.normal_axis `Z);
                  }
  in
  {shapes; center; half_size;}

let rebuild collidable = build collidable.shapes

