(**
   This module provide calculating voronoi region for a triangle, projected point on a plane and
   judgement if projected point is in voronoi region.

   @author derui
   @version 0.1
*)

(** type of a voronoi region *)
type t
type recent_type = Point of Candyvec.Vector.t
                   | Edge of Candyvec.Vector.t * Candyvec.Vector.t
                   | Shape of Candyvec.Vector.t

module Base : sig
  type t
  type triangle = Candyvec.Vector.t * Candyvec.Vector.t * Candyvec.Vector.t

  val projection_point : base:Candyvec.Vector.t -> normal:Candyvec.Vector.t ->
    point:Candyvec.Vector.t -> Candyvec.Vector.t
  (** Get vector projected on the point is contained the triangle. *)

  val make_region : triangle -> t list
  (** Make voronoi's regions with vertices of a some triangle.
      What regions are returned contains six regions, three regions are points and three regions
      are edges.

      @param triangle A triangle to make regions
      @return Regions are made with triangle given.
  *)

  val recent_region : region:t list -> point:Candyvec.Vector.t -> recent_type
(** Get type of recent region for the voronoi region given.
    If this function can not detect region is recent with given point, raise
    Exception.

    @param region A region to detect region is most recent to given point
    @param point A point to detect region is most recent to.
    @return type of region is most recent to given point
*)

end

val voronoi_region :Mesh.t -> int -> t
(** Calculate voronoi regions with vertices of a some shape.
    Aligns of vertices needs to be counter clockwise.

    @param mesh a mesh containing given facet to calculate voronoi regions
    @param facet number of a facet in given mesh
    @return vonoroi regions are calculated with given vertices
*)

val recent_of_region : Candyvec.Vector.t -> t -> recent_type
(** Get a type of recent point in the voronoi regions with a given point.

    @param point a point to get type of recent point in the voronoi regions
    @param regions voronoi regions of the shape to calculate type of recent point with a point
    @return type of recent point
*)
