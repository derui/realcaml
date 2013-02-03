(**
   this module provide matrix type and operations for 3x3 matrix.
   3D coordinates style of OpenGL is 'right-handed coordinates
   system'.
   Therefore, matrix operation conjections needs to apply for 'right
   to left' direction.
   One more attention to matrix conjection, OpenGL is using 'column
   vector' when apply matrix to vector.

   vector multiplying by matrix as below.

   | 11 | 12 | 13 || x |
   | 21 | 22 | 23 || y |
   | 31 | 32 | 33 || z |

   type of matrix providing this module is able to convert between
   array and it. User wouldn't careful inner implementation!
   (Of course, you could look .ml file if you wish)

   Matrix provided this module is able to convert to string, converted
   string format as row major order is as follows.
   | 11 | 12 | 13 |
   | 21 | 22 | 23 |
   | 31 | 32 | 33 |
   Therefore, sorry to above format can not change from user...

   3x3 orthoganal matrix do not use as translated transformation matrix, then
   you use 4x4 matrix need translated transformation matrix.
*)

(** precise 3x3 matrix definition.
    all attributes are mutable but not usually use this.
    The first number of each elements is row, and the second number of they is
    column.
*)
type t = {
  mutable m11 : float; mutable m12 : float; mutable m13 : float;
  mutable m21 : float; mutable m22 : float; mutable m23 : float;
  mutable m31 : float; mutable m32 : float; mutable m33 : float;
}

(** Type of conversion when matrix applies to convert to array.
    Row is row major order, and Column is column major order.
*)
type conversion_order = Row | Column

(** given matrix convert to primitive array.
    matrix of result is right hand coodinates and conjection right to
    left, and most warning point is to only multiply by `column vector'!

    @param order A order to convert matrix to array. Row is default.
    @param matrix A matrix to convert to array
    @return array A array is converted from matrix as given order
*)
val to_array : ?order:conversion_order -> t -> float array

(** construct a identity matrix.
    A identity matrix is unchanged matrix that multiply any matrix.
*)
val identity : unit -> t

(** multiply two matrices.
    To multiply direction is m1 to m2 that is equal to [m1][m2].

    @param m1 first matrix to multiply
    @param m2 second matrix to multiply.
*)
val multiply : m1:t -> m2:t -> t

(** create a rotation matrix defined by a rotation axis and a rotation
    angle.
    created matrix from this function is `eular angle` based rotation
    matrix.
    if using quatanion rotation matrix, use module of `quatanion` module.
*)
val rotation_matrix_of_axis : dir:Vector.t -> angle:float -> t

(** create a scaling matrix defined a vector that each values are
    what scaling is along the axis.
*)
val scaling : Vector.t -> t

(** multiply given vector with given matrix. *)
val mult_vec: mat:t -> vec:Vector.t -> Vector.t

(** construct inverse matrix.
    inverse matrix is usually used to unprojection matrix that
    translate position translated by projection matrix to  world
    position.

    inverse matrix isn't exists always. if inverse matrix haven't ,
    return None.
*)
val inverse : t -> t option

(** construct transpose matrix of given.
    One more transpose transposed matrix, it is equlivalence original one.
*)
val transpose : t -> t

(** Convert matrix to string  *)
val to_string: t -> string
