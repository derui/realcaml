(**
   This module provide calculating voronoi region for a triangle.

   @author derui
   @version 0.1
*)

(** type of a voronoi region *)
type t

val make: Types.triangle -> t
  (** [make_region triangle] make voronoi's regions with vertices of a some triangle.
      What regions are returned contains six regions, three regions are points and three regions
      are edges.
      [triangle] must be aligned to counter-clockwise of a triangle.
  *)

val recent_of_region : point:Types.vec -> t -> Types.recent_type
(** Get a type of recent point in the voronoi regions with a given point.

    @param point a point to get type of recent point in the voronoi regions
    @param regions voronoi regions of the shape to calculate type of recent point with a point
    @return type of recent point
*)
