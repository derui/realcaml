(**
   This module provide calculating voronoi region for a triangle.

   @author derui
   @version 0.1
*)

(** type of a voronoi region *)
module Edge_region = Realcaml_voronoi_edge_region

module Point_region = Realcaml_voronoi_point_region

module Plane_region = Realcaml_voronoi_plane_region

type region_type = [ `Edge of Edge_region.Region.t | `Point of Point_region.Region.t |
    `Plane of Plane_region.Region.t]

type t
(* The type of voronoi region *)

type triangle = Realcaml_util.vec * Realcaml_util.vec * Realcaml_util.vec
(* The type of triangle. *)

val make: triangle -> t list
(** [make_region triangle] make voronoi's regions with vertices of a some triangle.
    What regions are returned contains six regions, three regions are points and three regions
    are edges.
    [triangle] must be aligned to counter-clockwise of a triangle.
*)

val to_region_type: t -> region_type
(* [to_region_type region] get the region as [region_type].*)

