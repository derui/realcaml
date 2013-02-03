  (**
     Define facet data and operations of a Mesh module.

     @version 0.1
     @author derui
  *)

type t

val vertex_ids: t -> int * int * int
val edge_ids: t -> int * int * int
val normal: t -> Vecmath.Vector.t
  (** Get informations of a Facet data *)

  (** Make facet data  *)
val make: vertex_ids:int * int * int -> edge_ids:int * int * int -> normal:Vecmath.Vector.t -> t
