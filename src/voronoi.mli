(**
   This module provide calculating voronoi region for a triangle, projected point on a plane and
   judgement if projected point is in voronoi region.

   @author derui
   @version 0.1
*)

(** type of a voronoi region *)
type t

type recent_type = Point of Vecmath.Vector.t
                   | Edge of Vecmath.Vector.t * Vecmath.Vector.t
                   | Shape

(** Calculate voronoi regions with vertices of a some shape.
    Aligns of vertices needs to be counter clockwise.

    @param mesh a mesh containing given facet to calculate voronoi regions
    @param facet number of a facet in given mesh
    @return vonoroi regions are calculated with given vertices
*)
val voronoi_region :Mesh.t -> int -> t

(** Get a type of recent point in the voronoi regions with a given point.

    @param point a point to get type of recent point in the voronoi regions
    @param regions voronoi regions of the shape to calculate type of recent point with a point
    @return type of recent point
*)
val recent_of_region : Vecmath.Vector.t -> t -> recent_type
