(**
   Providing mesh data and operations to detect collision. Provided mesh data is always convex mesh.

   @version 0.1
   @author derui
*)

type t
(** type of convex mesh  *)

val max_vertices: int
val max_edges: int
val max_facets: int
(** Constants for convex mesh. These are limitation of Mesh made from this module.  *)

(** Convert vertices and vertex indexed faces. Given original data must necessary
    convex mesh, and a front face of given faces is counter-clockwise which vertices consisted of
    face is.
*)
val convert: vertices:Vecmath.Vector.t array -> faces:(int * int * int) array -> t

(** Get vertices consisted of mesh.  *)
val vertices: t -> Vecmath.Vector.t array

(** Get edges consists of mesh  *)
val edges : t -> Mesh_edge.t array

(** Get facets consists of mesh  *)
val facets: t -> Mesh_facet.t array
