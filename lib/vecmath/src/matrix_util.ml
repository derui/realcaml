
let to_4x4 (mat : Matrix3.t) : Matrix4.t =
  let module M = Matrix4 in
  let module M3 = Matrix3 in
  {
    M.m11 = mat.M3.m11; m12 = mat.M3.m12; m13 = mat.M3.m13; m14 = 0.0;
    m21 = mat.M3.m21; m22 = mat.M3.m22; m23 = mat.M3.m23; m24 = 0.0;
    m31 = mat.M3.m31; m32 = mat.M3.m32; m33 = mat.M3.m33; m34 = 0.0;
    m41 = 0.0; m42 = 0.0; m43 = 0.0; m44 = 0.0;
  }
