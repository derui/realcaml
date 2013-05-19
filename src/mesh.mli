(**
   Providing mesh data and operations to detect collision. Provided mesh data is always convex mesh.

   @version 0.1
   @author derui
*)


module Edge : sig
  type edge_type = Convex | Concave | Flat

  (** A type of Edge  *)
  type t = {
    edge_type:edge_type;
    vertex_ids:int * int;
    face_ids: int list;
  }
end

module Facet : sig
  type t = {
    vertex_ids:int * int * int;
    edge_ids: int * int * int;
    normal:Candyvec.Vector.t;
  }

end

(** type of convex mesh  *)
type t = {
  edges: Edge.t array;
  vertices: Candyvec.Vector.t array;
  facets:Facet.t array;
}
  
(** Convert vertices and vertex indexed faces. Given original data must necessary
    convex mesh, and a front face of given faces is counter-clockwise which vertices consisted of
    face is.
*)
val convert: vertices:Candyvec.Vector.t array -> faces:(int * int * int) array -> t

(** Return new mesh is transformed by given transformation matrix. *)
val transform_vertices: t -> Candyvec.Matrix4.t -> t
