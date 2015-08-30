(**
   This module provide calculating voronoi region for a triangle.

   @author derui
   @version 0.1
*)

(** type of a voronoi region *)
module Edge_region : sig
  (* TRANSLATE: エッジの始点と終点、エッジの法線ベクトル、面の法線ベクトル *)
  type t = {
    edge: Types.vec * Types.vec;
    normal: Types.vec;
    face_normal: Types.vec;
  }
end

module Point_region : sig
  (* TRANSLATE: 基準となる点と、点から伸びるエッジそれぞれの法線ベクトル、面の法線ベクトル *)
  type t = {
    base: Types.vec;
    a_normal: Types.vec;
    b_normal: Types.vec;
    normal: Types.vec;
  }
end

type region_type = [ `Edge of Edge_region.t | `Point of Point_region.t]
type t

val make: Types.triangle -> t list
(** [make_region triangle] make voronoi's regions with vertices of a some triangle.
    What regions are returned contains six regions, three regions are points and three regions
    are edges.
    [triangle] must be aligned to counter-clockwise of a triangle.
*)

val to_region_type: t -> region_type
(* [to_region_type region] get the region as [region_type].*)

