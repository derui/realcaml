(**
   Define edge of facet in a Mesh module.

   @version 0.1
   @author derui
*)

type edge_type = Convex | Concave | Flat

  (** A type of Edge  *)
type t

val edge_type: t -> edge_type
val vertex_ids: t -> int * int
val face_ids: t -> int list
  (** Get infomation of a Edge type  *)

val make: etype:edge_type -> vertex_ids:int * int -> face_ids:int list -> t
(** Make edge data. *)
