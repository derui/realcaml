open Vector

let cross_matrix v =
  let module M = Matrix3 in
  {M.m11 = 0.0; m12 = -.v.z; m13 = v.x;
   m21 = v.z; m22 = 0.0; m23 = -.v.x;
   m31 = -.v.y; m32 = v.x; m33 = 0.0;
  }
