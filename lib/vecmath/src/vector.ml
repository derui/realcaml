type t = {x:float; y:float; z:float}

let zero = {x = 0.0; y = 0.0; z = 0.0}

let of_vec {x;y;z} = (x, y, z)

let normal_axis = function
  | `X -> {x = 1.0; y = 0.0; z = 0.0}
  | `Y -> {x = 0.0; y = 1.0; z = 0.0}
  | `Z -> {x = 0.0; y = 0.0; z = 1.0}

let norm_square {x;y;z} = x *. x +. y *. y +. z *. z
let norm {x;y;z} = sqrt(x *. x +. y *. y +. z *. z)

let normalize {x;y;z} =
  let len = norm {x;y;z} in
  {x = x /. len; y = y /. len;z = z /. len}

let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let cross v1 v2 =
  {x = v1.y *. v2.z -. v1.z *. v2.y;
   y = v1.z *. v2.x -. v1.x *. v2.z;
   z = v1.x *. v2.y -. v1.y *. v2.x;
  }

let scale ~v ~scale =
  {x = v.x *. scale; y = v.y *. scale; z = v.z *. scale}

let add v1 v2 = {x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z}

let sub v1 v2 = {x = v1.x -. v2.x;y = v1.y -. v2.y; z = v1.z -. v2.z}

let is_square v1 v2 =
  match classify_float (dot v1 v2) with
      FP_zero -> true
    | _ -> false

let invert v = scale ~v ~scale:(-1.0)

(* Compare two vector.  *)
let compare v1 v2 =
  let module F = Baselib.Std.Float in
  let c_x = F.compare v1.x v2.x
  and c_y = F.compare v1.y v2.y
  and c_z = F.compare v1.z v2.z in

  if c_x = 0 then
    if c_y = 0 then
      if c_z = 0 then 0
      else c_z
    else c_y
  else c_x
