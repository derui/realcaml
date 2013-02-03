module M = Matrix4
module V = Vector

type t = {
  quat_angle:float;
  quat_axis:V.t;
}

let pi = 3.1415926535897232

let epsilon = 0.00001

let identity _ =
  {quat_angle = 1.0;
   quat_axis = V.zero
  }

let make ~angle ~vector =
  let angle = angle /. 2.0 in
  let radian = pi /. 180.0 *. angle in
  let base_sin = sin radian  in
  let normal = V.normalize vector in
  {quat_angle = cos radian;
   quat_axis = V.scale ~v:normal ~scale:base_sin
  }

let norm {quat_axis;quat_angle} =
  let presquare = quat_angle *. quat_angle +.
    (V.norm_square quat_axis) in
  sqrt presquare

let normalize quat =
  let normed = norm quat in
  (* norm should be over 0.0 *)
  if normed > 0.0 then
    let mangle = 1.0 /. normed in
    {quat_angle = quat.quat_angle *. mangle;
     quat_axis = V.scale ~v:quat.quat_axis ~scale:mangle
    }
  else
    identity ()

let multiply first second =
  let w1 = first.quat_angle
  and (x1, y1, z1) = V.of_vec first.quat_axis
  and w2 = second.quat_angle
  and (x2, y2, z2) = V.of_vec second.quat_axis in
  { quat_angle = w1 *. w2 -. x1 *. x2 -. y1 *. y2 -. z1 *. z2;
    quat_axis =
      { V.x = w1 *. x2 +. x1 *. w2 +. y1 *. z2 -. z1 *. y2;
        y = w1 *. y2 +. y1 *. w2 +. z1 *. x2 -. x1 *. z2;
        z =w1 *. z2 +. z1 *. w2 +. x1 *. y2 -. y1 *. x2;
      }
  }

let ( *> ) first second = multiply first second
let ( *< ) first second = multiply second first

let angle {quat_angle;_} = 180.0 /. pi *. 2.0 *. (acos quat_angle)
let axis {quat_axis;quat_angle} =
  let sin_theta_sq = 1.0 -. quat_angle *. quat_angle in
  (* if calculated sin is lesser than zero, return zero vector *)
  if sin_theta_sq <= 0.0 then
    (identity ()).quat_axis
  else
    let over_sin = 1.0 /. sqrt sin_theta_sq in
    V.scale ~v:quat_axis ~scale:over_sin

let to_matrix quat =
  let w = quat.quat_angle
  and (x, y, z) = V.of_vec quat.quat_axis in
  {
    (* for x row vector *)
    M.m11 = 1.0 -. (2.0 *. y *. y) -. (2.0 *. z *. z);
    M.m12 = (2.0 *. x *. y) +. (2.0 *. w *. z);
    M.m13 = (2.0 *. x *. z) -. (2.0 *. w *. y);
    M.m14 = 0.0;
    (* for y row vector *)
    M.m21 = (2.0 *. x *. y) -. (2.0 *. w *. z);
    M.m22 = 1.0 -. (2.0 *. x *. x) -. (2.0 *. z *. z);
    M.m23 = (2.0 *. y *. z) +. (2.0 *. w *. x);
    M.m24 = 0.0;
    (* for z row vector *)
    M.m31 = (2.0 *. x *. z) +. (2.0 *. w *. y);
    M.m32 = (2.0 *. y *. z) -. (2.0 *. w *. x);
    M.m33 = 1.0 -. (2.0 *. x *. x) -. (2.0 *. y *. y);
    M.m34 = 0.0;
    (* for z row vector *)
    M.m41 = 0.0; M.m42 = 0.0; M.m43 = 0.0; M.m44 = 1.0;
  }

(* construct dot product from q1 and q2. *)
let dot q1 q2 =
  let w = q1.quat_angle *. q2.quat_angle
  and (x1, y1, z1) = V.of_vec q1.quat_axis
  and (x2, y2, z2) = V.of_vec q2.quat_axis in
  w +. (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

(* return tuple quaternions between q1 and q2.
   if q1 and q2 given quaternion's dot is negate, return
   (-q1, q2). q2 is without change always.
*)
let minimum_angle q1 q2 =
  let dotted = dot q1 q2 in
  if dotted < 0.0 then
    ({quat_angle = -.dotted; quat_axis = V.scale ~v:q1.quat_axis ~scale:(-1.0)}, q2)
  else
    (q1, q2)

let slerp ~from_quat ~to_quat ~freq =
  let (from_quat, to_quat) = minimum_angle from_quat to_quat in
  let dotted = dot from_quat to_quat in
  let correction_freq freq dotted =
    (* calculate correction ratio from from-to frequently.
       However, if from and to are almost to point equalivalence
       direction, return liner correction ratio.
    *)
    if dotted > (1.0 -. epsilon) then
      (1.0 -. freq, freq)
    else
      let sin_dot = sqrt (1.0 -. dotted *. dotted) in
      let theta = atan2 sin_dot dotted in
      let over = 1.0 /. sin_dot in
      ((sin ((1.0 -. freq) *. theta)) *. over,
       (sin (freq *. theta)) *. over
      )
  in
  let (k0, k1) = correction_freq freq dotted in
  let w0 = from_quat.quat_angle
  and (x0,y0,z0) = V.of_vec from_quat.quat_axis
  and w1 = to_quat.quat_angle
  and (x1,y1,z1) = V.of_vec to_quat.quat_axis
  in
  {quat_angle = w0 *. k0 +. w1 *. k1;
   quat_axis = {V.x = x0 *. k0 +. x1 *. k1;
                y = y0 *. k0 +. y1 *. k1;
                z = z0 *. k0 +. z1 *. k1}
  }
