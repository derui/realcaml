(**
   this module provide matrix type and operations for OpenGL.
   3D coordinates style of OpenGL is 'right-handed coordinates
   system'.
   Therefore, matrix operation conjections needs to apply for 'right
   to left' direction.
   One more attention to matrix conjection, OpenGL is using 'column
   vector' when apply matrix to vector.

   vector multiplying by matrix as below.

   | 11 | 12 | 13 | 14 || x |
   | 21 | 22 | 23 | 24 || y |
   | 31 | 32 | 33 | 34 || z |
   | 41 | 42 | 43 | 44 || w |

   type of matrix providing this module is able to convert between
   array and it. User wouldn't careful inner implementation!
   (Of course, you could look .ml file if you wish)

   Matrix provided this module is able to convert to string, converted
   string format as row major order is as follows.
   | 11 | 12 | 13 | 14 |
   | 21 | 22 | 23 | 24 |
   | 31 | 32 | 33 | 34 |
   | 41 | 42 | 43 | 44 |
   Therefore, sorry to above format can not change from user...
*)

(** precise 4x4 matrix definition.
    all attributes are mutable but not usually use this.
    The first number of each elements is row, and the second number of they is
    column.
*)
type t = {
  mutable m11 : float; mutable m12 : float; mutable m13 : float; mutable m14 : float;
  mutable m21 : float; mutable m22 : float; mutable m23 : float; mutable m24 : float;
  mutable m31 : float; mutable m32 : float; mutable m33 : float; mutable m34 : float;
  mutable m41 : float; mutable m42 : float; mutable m43 : float; mutable m44 : float;
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

(**
   construct a projection matrix for `ortho projection'.
   given parameters of near and far are mostly equivalant of
   perspective_projection arguments.
   Left and right specifies the coordinates for the left and right vertical clipping planes.
   Bottom and top specifies the coodinates for the left and right horizontal clipping planes.
*)
val ortho_projection : left:float -> right:float ->
  top:float -> bottom:float -> near:float -> far:float -> t

(**
   construct a projection matrix.
   `fov' is that the field of view angle in the y direction.
   `ratio` is screen ratio that determines the field of view in the x direction.
   near and far is clip that visible range along the Z axis.

   @param fov Field Of View
   @param the ratio of x to y
   @param near near clip
   @param far far clip
*)
val perspective_projection :
  fov:float ->
  ratio:float ->
  near:float ->
  far:float -> t

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

(** create a translation matrix defined a vector is length of moving.  *)
val translation : Vector.t -> t

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

(** Convert matrix to string *)
val to_string: t -> string
