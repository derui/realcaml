(**
   This module provide some operations for segment and line.

   @author derui
   @since 0.1
*)

(** Type of segment. *)
type t

(**
   Make segment with start and last vector as point.
   Note : segment made by this function is parametric style.

   @param start Vector to make segment for start
   @param last Vector to make segment for last
   @return segment is made by vectors are given
*)
val make : start:Vector.t -> last:Vector.t -> t

(**
   Get closest point on the segment for given point.

   @param segment A segment to get closest point for given point
   @param point A point to get closest point on the segment.
   @return closest point on the segment for given point.
*)
val closest_with_point : t -> Vector.t -> Vector.t

(**
   Get closest point between two segment, and point returned this function
   is on the second segment from on the first segment.

   @param first first segment to get closest point
   @param second second segment to get closest point.
   @return
*)
val closest : t -> t -> Vector.t * Vector.t
