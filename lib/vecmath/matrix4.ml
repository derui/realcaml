module V = Vector

(* precise 4x4 matrix definition. *)
type t = {
  mutable m11 : float; mutable m12 : float; mutable m13 : float; mutable m14 : float;
  mutable m21 : float; mutable m22 : float; mutable m23 : float; mutable m24 : float;
  mutable m31 : float; mutable m32 : float; mutable m33 : float; mutable m34 : float;
  mutable m41 : float; mutable m42 : float; mutable m43 : float; mutable m44 : float;
}

type conversion_order = Row | Column

let to_array ?(order=Row) mat =
  match order with
  | Row ->
    [|
      mat.m11; mat.m12; mat.m13; mat.m14;
      mat.m21; mat.m22; mat.m23; mat.m24;
      mat.m31; mat.m32; mat.m33; mat.m34;
      mat.m41; mat.m42; mat.m43; mat.m44;
    |]
  | Column ->
    [|
      mat.m11; mat.m21; mat.m31; mat.m41;
      mat.m12; mat.m22; mat.m32; mat.m42;
      mat.m13; mat.m23; mat.m33; mat.m43;
      mat.m14; mat.m24; mat.m34; mat.m44;
    |]

(* convert type of t to array have two dimension as 4 * 4.
   The first index of the converted array is row, and
   the two index of the converted array is column.
*)
let to_matrix_array mat =
  [|
    [|mat.m11; mat.m12; mat.m13; mat.m14;|];
    [|mat.m21; mat.m22; mat.m23; mat.m24;|];
    [|mat.m31; mat.m32; mat.m33; mat.m34;|];
    [|mat.m41; mat.m42; mat.m43; mat.m44;|];
  |]

let identity () =
  {
    m11 = 1.0; m12 = 0.0; m13 = 0.0; m14 = 0.0;
    m21 = 0.0; m22 = 1.0; m23 = 0.0; m24 = 0.0;
    m31 = 0.0; m32 = 0.0; m33 = 1.0; m34 = 0.0;
    m41 = 0.0; m42 = 0.0; m43 = 0.0; m44 = 1.0;
  }

let ortho_projection ~left ~right ~top ~bottom ~near ~far =
  let width_ratio = 2.0 /. (right -. left)
  and height_ratio = 2.0 /. (top -. bottom)
  and z_ratio = -2.0 /. (far -. near)
  and tx = -1.0 *. (right +. left) /. (right -. left)
  and ty = -1.0 *. (top +. bottom) /. (top -. bottom)
  and tz = -1.0 *. (far +. near) /. (far -. near) in

  { m11 = width_ratio ; m12 = 0.0;          m13 = 0.0; m14 = tx;
    m21 = 0.0         ; m22 = height_ratio; m23 = 0.0; m24 = ty;
    m31 = 0.0         ; m32 = 0.0;          m33 = z_ratio; m34 = tz;
    m41 = 0.0         ; m42 = 0.0;          m43 = 0.0; m44 = 1.0;
  }

let perspective_projection ~fov ~ratio ~near ~far =
  let pi = 3.14159265358979323846 in
  let maxY = sin (fov /. 2.0 *. pi /. 180.0) in
  let minY = -. maxY in
  let minX = minY /. ratio
  and maxX = maxY /. ratio in

  let x_diff = maxX -. minX in
  let y_diff = maxY -. minY in
  let z_diff = far -. near in
  let near_twice = 2.0 *. near in

  let a = near_twice /. x_diff
  and b = near_twice /. y_diff
  and c = (maxX +. minX) /. x_diff
  and d = (maxY +. minY) /. y_diff
  and e = -. (far +. near) /. z_diff
  and f = -. (near_twice *. far) /. z_diff
  in
  { m11 = a;   m12 = 0.0; m13 = c; m14 =  0.0;
    m21 = 0.0; m22 = b;   m23 = d; m24 =  0.0;
    m31 = 0.0; m32 = 0.0; m33 = e;   m34 = f;
    m41 = 0.0; m42 = 0.0; m43 = -1.0;   m44 =  0.0;
  }

let multiply ~m1 ~m2 =
  {
    m11 = m1.m11 *. m2.m11 +. m1.m12 *. m2.m21 +. m1.m13 *. m2.m31 +. m1.m14 *. m2.m41;
    m12 = m1.m11 *. m2.m12 +. m1.m12 *. m2.m22 +. m1.m13 *. m2.m32 +. m1.m14 *. m2.m42;
    m13 = m1.m11 *. m2.m13 +. m1.m12 *. m2.m23 +. m1.m13 *. m2.m33 +. m1.m14 *. m2.m43;
    m14 = m1.m11 *. m2.m14 +. m1.m12 *. m2.m24 +. m1.m13 *. m2.m34 +. m1.m14 *. m2.m44;
    m21 = m1.m21 *. m2.m11 +. m1.m22 *. m2.m21 +. m1.m23 *. m2.m31 +. m1.m24 *. m2.m41;
    m22 = m1.m21 *. m2.m12 +. m1.m22 *. m2.m22 +. m1.m23 *. m2.m32 +. m1.m24 *. m2.m42;
    m23 = m1.m21 *. m2.m13 +. m1.m22 *. m2.m23 +. m1.m23 *. m2.m33 +. m1.m24 *. m2.m43;
    m24 = m1.m21 *. m2.m14 +. m1.m22 *. m2.m24 +. m1.m23 *. m2.m34 +. m1.m24 *. m2.m44;
    m31 = m1.m31 *. m2.m11 +. m1.m32 *. m2.m21 +. m1.m33 *. m2.m31 +. m1.m34 *. m2.m41;
    m32 = m1.m31 *. m2.m12 +. m1.m32 *. m2.m22 +. m1.m33 *. m2.m32 +. m1.m34 *. m2.m42;
    m33 = m1.m31 *. m2.m13 +. m1.m32 *. m2.m23 +. m1.m33 *. m2.m33 +. m1.m34 *. m2.m43;
    m34 = m1.m31 *. m2.m14 +. m1.m32 *. m2.m24 +. m1.m33 *. m2.m34 +. m1.m34 *. m2.m44;
    m41 = m1.m41 *. m2.m11 +. m1.m42 *. m2.m21 +. m1.m43 *. m2.m31 +. m1.m44 *. m2.m41;
    m42 = m1.m41 *. m2.m12 +. m1.m42 *. m2.m22 +. m1.m43 *. m2.m32 +. m1.m44 *. m2.m42;
    m43 = m1.m41 *. m2.m13 +. m1.m42 *. m2.m23 +. m1.m43 *. m2.m33 +. m1.m44 *. m2.m43;
    m44 = m1.m41 *. m2.m14 +. m1.m42 *. m2.m24 +. m1.m43 *. m2.m34 +. m1.m44 *. m2.m44;
  }

let mult_vec ~mat ~vec =
  let open Vector in
  { x = mat.m11 *. vec.x +. mat.m12 *. vec.y +. mat.m13 *. vec.z +. mat.m14;
    y = mat.m21 *. vec.x +. mat.m22 *. vec.y +. mat.m23 *. vec.z +. mat.m24;
    z = mat.m31 *. vec.x +. mat.m32 *. vec.y +. mat.m33 *. vec.z +. mat.m34;
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
  { m11 = 1.0 -. 2.0 *. (y2 +. z2) ; m12 = 2.0 *. (xy -. wz)        ;  m13 = 2.0 *. (xz +. wy)        ; m14 = 0.0;
    m21 = 2.0 *. (xy +. wz)        ; m22 = 1.0 -. 2.0 *. (x2 +. z2) ;  m23 = 2.0 *. (yz -. wx)        ; m24 = 0.0;
    m31 = 2.0 *. (xz -. wy)        ; m32 = 2.0 *. (yz +. wx)        ;  m33 = 1.0 -. 2.0 *. (x2 +. y2) ; m34 = 0.0;
    m41 = 0.0                      ; m42 = 0.0                      ;  m43 = 0.0                      ; m44 = 1.0;
  }


let translation v =
  let x, y, z = V.of_vec v in
  { m11 = 1.0; m12 = 0.0; m13 = 0.0; m14 = x;
    m21 = 0.0; m22 = 1.0; m23 = 0.0; m24 = y;
    m31 = 0.0; m32 = 0.0; m33 = 1.0; m34 = z;
    m41 = 0.0; m42 = 0.0; m43 = 0.0; m44 = 1.0;
  }

let scaling v =
  let x, y, z = V.of_vec v in
  { m11 =   x; m12 = 0.0; m13 = 0.0; m14 = 0.0;
    m21 = 0.0; m22 =   y; m23 = 0.0; m24 = 0.0;
    m31 = 0.0; m32 = 0.0; m33 =   z; m34 = 0.0;
    m41 = 0.0; m42 = 0.0; m43 = 0.0; m44 = 1.0;
  }

(* To use is only internal.  *)
let divide m d =
  {
    m11 = m.m11 /. d; m12 = m.m12 /. d; m13 = m.m13 /. d; m14 = m.m14 /. d;
    m21 = m.m21 /. d; m22 = m.m22 /. d; m23 = m.m23 /. d; m24 = m.m24 /. d;
    m31 = m.m31 /. d; m32 = m.m32 /. d; m33 = m.m33 /. d; m34 = m.m34 /. d;
    m41 = m.m41 /. d; m42 = m.m42 /. d; m43 = m.m43 /. d; m44 = m.m44 /. d;
  }

(* providing matrix from this module is always cator-cornred. *)
let transpose mat =
  {
    m11 = mat.m11; m12 = mat.m21; m13 = mat.m31; m14 = mat.m41;
    m21 = mat.m12; m22 = mat.m22; m23 = mat.m32; m24 = mat.m42;
    m31 = mat.m13; m32 = mat.m23; m33 = mat.m33; m34 = mat.m43;
    m41 = mat.m14; m42 = mat.m24; m43 = mat.m34; m44 = mat.m44;
  }

(* calculate determinant of the matrix  *)
let determine m =
  let deter1_1 = m.m22 *. (m.m33 *. m.m44 -. m.m34 *. m.m43)
  and deter1_2 = m.m32 *. (m.m23 *. m.m44 +. m.m24 *. m.m43)
  and deter1_3 = m.m42 *. (m.m23 *. m.m34 -. m.m24 *. m.m33)
  and deter2_1 = m.m12 *. (m.m33 *. m.m44 -. m.m34 *. m.m43)
  and deter2_2 = m.m32 *. (m.m13 *. m.m44 +. m.m14 *. m.m43)
  and deter2_3 = m.m42 *. (m.m13 *. m.m24 -. m.m14 *. m.m33)
  and deter3_1 = m.m12 *. (m.m23 *. m.m44 -. m.m24 *. m.m43)
  and deter3_2 = m.m22 *. (m.m13 *. m.m44 +. m.m14 *. m.m43)
  and deter3_3 = m.m42 *. (m.m13 *. m.m24 -. m.m14 *. m.m23)
  and deter4_1 = m.m12 *. (m.m23 *. m.m44 -. m.m24 *. m.m33)
  and deter4_2 = m.m22 *. (m.m13 *. m.m34 +. m.m14 *. m.m33)
  and deter4_3 = m.m32 *. (m.m13 *. m.m24 -. m.m14 *. m.m23) in
  m.m11 *. (deter1_1 +. (-1.0 *. deter1_2) +. deter1_3) -.
    m.m21 *. (deter2_1 +. (-1.0 *. deter2_2) +. deter2_3) +.
    m.m31 *. (deter3_1 +. (-1.0 *. deter3_2) +. deter3_3) -.
    m.m41 *. (deter4_1 +. (-1.0 *. deter4_2) +. deter4_3)

(* calculate determinant of the matrix as 3 x 3 *)
let determine3x3 m =
  m.(1).(1) *. (m.(2).(2) *. m.(3).(3) -. m.(2).(3) *. m.(3).(2)) +.
    m.(1).(2) *. (m.(2).(3) *. m.(3).(1) -. m.(2).(1) *. m.(3).(3)) +.
    m.(1).(3) *. (m.(2).(1) *. m.(3).(2) -. m.(2).(2) *. m.(3).(1))

(* calculate cofactor of the a element at specified row and column in the matrix *)
let cofactor (m:float array array) row col =
  let get_index n = List.filter (fun i -> i <> n) [1;2;3;4] in
  let row_index = get_index row
  and col_index = get_index col in
  let pre_matrix = List.fold_left (fun l row ->
    (List.fold_left (fun l col ->
      m.(row).(col) :: l
    ) [] col_index) :: l
  ) [] row_index in
  let matrix = Array.of_list (List.map Array.of_list pre_matrix) in

  (-1.0) ** (float row +. float col) *. determine3x3 matrix

(* create adjoint of the given 4x4 matrix *)
let adjoint m =
  let m = Array.copy m in
  let indecies = [1;2;3;4] in
  List.iter (fun row ->
    List.iter (fun col ->
      m.(row).(col) <- cofactor m row col
    ) indecies
  ) indecies;
  let m =
    {
      m11 = m.(1).(1); m12 = m.(2).(1); m13 = m.(3).(1); m14 = m.(4).(1);
      m21 = m.(1).(2); m22 = m.(2).(2); m23 = m.(3).(2); m24 = m.(4).(2);
      m31 = m.(1).(3); m32 = m.(2).(3); m33 = m.(3).(3); m34 = m.(4).(3);
      m41 = m.(1).(4); m42 = m.(2).(4); m43 = m.(3).(4); m44 = m.(4).(4);
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
  Printf.sprintf "| %f | %f | %f | %f |\n" mat.m11 mat.m12 mat.m13 mat.m14 ^
    Printf.sprintf "| %f | %f | %f | %f |\n" mat.m21 mat.m22 mat.m23 mat.m24 ^
    Printf.sprintf "| %f | %f | %f | %f |\n" mat.m31 mat.m32 mat.m33 mat.m34 ^
    Printf.sprintf "| %f | %f | %f | %f |\n" mat.m41 mat.m42 mat.m43 mat.m44
