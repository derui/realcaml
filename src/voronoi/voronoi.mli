(** This module provide calculating voronoi region for a triangle, projected point on a plane and judgement if projected
    point is in voronoi region.

    @author derui
    @version 0.1 *)

type t
(** type of a voronoi region *)

val voronoi_region : Realcaml_mesh.Mesh.t -> int -> t
(** Calculate voronoi regions with vertices of a some shape. Aligns of vertices needs to be counter clockwise.

    @param mesh a mesh containing given facet to calculate voronoi regions
    @param number of facet in given mesh
    @return vonoroi regions are calculated with given vertices *)

val recent_of_region : point:Realcaml_util.Types.vec -> t -> Realcaml_util.Types.vec
(** Get a type of recent point in the voronoi regions with a given point.

    @param point a point to get type of recent point in the voronoi regions
    @param regions voronoi regions of the shape to calculate type of recent point with a point
    @return recent point on regions *)
