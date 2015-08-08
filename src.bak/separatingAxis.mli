(**
   Defines data type and operations on separating axis theorem.
   Provided operations from this module are checked colision each convex meshes,
   and calculate penetrate depth if each convex meshes are collision.

   @version 0.1
   @author derui
*)

type depth = float

type separating_type = Edge             (* Edge to edge *)
                       | APlane         (* A plane of the body A *)
                       | BPlane         (* A plane of the body B *)

type separating_axis = separating_type * Candyvec.Vector.t * depth

type mesh_info = Mesh.t * Candyvec.Std.Matrix.t

(** A Module what is provided base functions for SeparationAxis module. *)
module Base : sig
  val get_maximum_range : Candyvec.Std.Vector.t -> Candyvec.Std.Vector.t array -> float * float
  (** Get minimum and maximum result by dotted vector.
      @return (maximum, minimum)
  *)


  val detect_separation : Candyvec.Std.Vector.t -> float * float -> float * float ->
    (Candyvec.Std.Vector.t * float) option
  (** Detect separating axis between objects. *)
end


val is_separate_axis : info_a:mesh_info -> info_b:mesh_info -> sep_axis:Candyvec.Std.Vector.t ->
  (Candyvec.Std.Vector.t * float) option
(** What checking separated to between two meshes are based on separate axis.

    Notice, sep_axis is located on the coodinate on the info_a local coodinate.

    @param info_a a mesh and world transform matrix to check separating with info_b.
    @param info_b a mesh and world transform matrix to check separating with info_a.
    @param sep_axis a separate axis to check separating two meshes.
*)

val judge_intersect: body_a:RigidBodyInfo.t -> body_b:RigidBodyInfo.t -> separating_axis option
(** Judge each meshes separeted or not. This funciton is based on
    separating axis theorem.
    Judging separation on the separating axis theorem targeted convex mesh is
    based on axis made by a vertex and face or a edge and a edge.

    @param mesh_a a mesh to use base coodinates judging each meshes.
    @param mesh_b a mesh to check separation each meshes.
    @return separating axis and penetrate depth if meshes are not separation, or return None.
*)
