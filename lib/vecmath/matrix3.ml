module V = Vector

(* precise 4x4 matrix definition. *)
type t = {
  mutable m11 : float; mutable m12 : float; mutable m13 : float;
  mutable m21 : float; mutable m22 : float; mutable m23 : float;
  mutable m31 : float; mutable m32 : float; mutable m33 : float;
}

type conversion_order = Row | Column

let to_array ?(order=Row) mat =
  match order with
  | Row ->
    [|
      mat.m11; mat.m12; mat.m13;
      mat.m21; mat.m22; mat.m23;
      mat.m31; mat.m32; mat.m33;
    |]
  | Column ->
    [|
      mat.m11; mat.m21; mat.m31;
      mat.m12; mat.m22; mat.m32;
      mat.m13; mat.m23; mat.m33;
    |]

(* convert type of t to array have two dimension as 4 * 4.
   The first index of the converted array is row, and
   the two index of the converted array is column.
*)
let to_matrix_array mat =
  [|
    [|mat.m11; mat.m12; mat.m13; |];
    [|mat.m21; mat.m22; mat.m23; |];
    [|mat.m31; mat.m32; mat.m33; |];
  |]

let identity () =
  {
    m11 = 1.0; m12 = 0.0; m13 = 0.0;
    m21 = 0.0; m22 = 1.0; m23 = 0.0;
    m31 = 0.0; m32 = 0.0; m33 = 1.0;
  }

let multiply ~m1 ~m2 =
  {
    m11 = m1.m11 *. m2.m11 +. m1.m12 *. m2.m21 +. m1.m13 *. m2.m31;
    m12 = m1.m11 *. m2.m12 +. m1.m12 *. m2.m22 +. m1.m13 *. m2.m32;
    m13 = m1.m11 *. m2.m13 +. m1.m12 *. m2.m23 +. m1.m13 *. m2.m33;
    m21 = m1.m21 *. m2.m11 +. m1.m22 *. m2.m21 +. m1.m23 *. m2.m31;
    m22 = m1.m21 *. m2.m12 +. m1.m22 *. m2.m22 +. m1.m23 *. m2.m32;
    m23 = m1.m21 *. m2.m13 +. m1.m22 *. m2.m23 +. m1.m23 *. m2.m33;
    m31 = m1.m31 *. m2.m11 +. m1.m32 *. m2.m21 +. m1.m33 *. m2.m31;
    m32 = m1.m31 *. m2.m12 +. m1.m32 *. m2.m22 +. m1.m33 *. m2.m32;
    m33 = m1.m31 *. m2.m13 +. m1.m32 *. m2.m23 +. m1.m33 *. m2.m33;
  }

let mult_vec ~mat ~vec =
  let open Vector in
  { x = mat.m11 *. vec.x +. mat.m12 *. vec.y +. mat.m13 *. vec.z;
    y = mat.m21 *. vec.x +. mat.m22 *. vec.y +. mat.m23 *. vec.z;
    z = mat.m31 *. vec.x +. mat.m32 *. vec.y +. mat.m33 *. vec.z;
  }

let rotation_matrix_of_axis ~dir ~angle =
  let angle = angle *. 0.5 in
  let vn_x, vn_y, vn_z = V.of_vec (V.normalize dir) in
  let sinAngle = sin angle in
  let qx = vn_x *. sinAngle
  and qy = vn_y *. sinAngle
  and qz = vn_z *. sinAngle
  and qw = cos angle
  in
  let x2 = qx *. qx
  and y2 = qy *. qy
  and z2 = qz *. qz
  and xy = qx *. qy
  and xz = qx *. qz
  and yz = qy *. qz
  and wx = qw *. qx
  and wy = qw *. qy
  and wz = qw *. qz in
  { m11 = 1.0 -. 2.0 *. (y2 +. z2) ; m12 = 2.0 *. (xy -. wz)        ;  m13 = 2.0 *. (xz +. wy)        ;
    m21 = 2.0 *. (xy +. wz)        ; m22 = 1.0 -. 2.0 *. (x2 +. z2) ;  m23 = 2.0 *. (yz -. wx)        ;
    m31 = 2.0 *. (xz -. wy)        ; m32 = 2.0 *. (yz +. wx)        ;  m33 = 1.0 -. 2.0 *. (x2 +. y2) ;
  }

let scaling v =
  let x, y, z = V.of_vec v in
  { m11 =   x; m12 = 0.0; m13 = 0.0;
    m21 = 0.0; m22 =   y; m23 = 0.0;
    m31 = 0.0; m32 = 0.0; m33 =   z;
  }

(* To use is only internal.  *)
let divide m d =
  {
    m11 = m.m11 /. d; m12 = m.m12 /. d; m13 = m.m13 /. d;
    m21 = m.m21 /. d; m22 = m.m22 /. d; m23 = m.m23 /. d;
    m31 = m.m31 /. d; m32 = m.m32 /. d; m33 = m.m33 /. d;
  }

(* providing matrix from this module is always cator-cornred. *)
let transpose mat =
  {
    m11 = mat.m11; m12 = mat.m21; m13 = mat.m31;
    m21 = mat.m12; m22 = mat.m22; m23 = mat.m32;
    m31 = mat.m13; m32 = mat.m23; m33 = mat.m33;
  }

(* calculate determinant of the matrix  *)
let determine m =
  let deter1 = m.m11 *. m.m22 *. m.m33
  and deter2 = m.m12 *. m.m23 *. m.m31
  and deter3 = m.m13 *. m.m21 *. m.m32
  and deter4 = -1.0 *. (m.m13 *. m.m22 *. m.m31)
  and deter5 = -1.0 *. (m.m12 *. m.m21 *. m.m33)
  and deter6 = -1.0 *. (m.m11 *. m.m23 *. m.m32) in
  deter1 +. deter2 +. deter3 +. deter4 +. deter5 +. deter6

let determine2 m = m.(1).(1) *. m.(2).(2) -. m.(2).(1) *. m.(1).(2)

let cofactor (m:float array array) row col =
  let get_index n = List.filter (fun i -> i <> n) [1;2;3] in
  let row_index = get_index row
  and col_index = get_index col in
  let pre_matrix = List.fold_left (fun l row ->
    (List.fold_left (fun l col ->
      m.(row).(col) :: l
    ) [] col_index) :: l
  ) [] row_index in
  let matrix = Array.of_list (List.map Array.of_list pre_matrix) in

  (-1.0) ** (float row +. float col) *. determine2 matrix

(* create adjoint of the given 4x4 matrix *)
let adjoint m =
  let m = Array.copy m in
  let indecies = [1;2;3] in
  List.iter (fun row ->
    List.iter (fun col ->
      m.(row).(col) <- cofactor m row col
    ) indecies
  ) indecies;
  let m =
    {
      m11 = m.(1).(1); m12 = m.(2).(1); m13 = m.(3).(1);
      m21 = m.(1).(2); m22 = m.(2).(2); m23 = m.(3).(2);
      m31 = m.(1).(3); m32 = m.(2).(3); m33 = m.(3).(3);
    } in
  transpose m

let inverse m =
  let deter = determine m in
  if deter = 0.0 then
    None
  else
    let array_m = to_matrix_array m in
    let adj = adjoint array_m in
    Some (divide adj deter)

let to_string mat =
  Printf.sprintf "| %f | %f | %f |\n" mat.m11 mat.m12 mat.m13 ^
    Printf.sprintf "| %f | %f | %f |\n" mat.m21 mat.m22 mat.m23 ^
    Printf.sprintf "| %f | %f | %f |\n" mat.m31 mat.m32 mat.m33
