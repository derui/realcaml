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

(** Judge each meshes separeted or not. This funciton is based on
    separating axis theorem.
    Judging separation on the separating axis theorem targeted convex mesh is
    based on axis made by a vertex and face or a edge and a edge.

    @param mesh_a a mesh to use base coodinates judging each meshes.
    @param mesh_b a mesh to check separation each meshes.
    @return separating axis and penetrate depth if meshes are not separation, or return None.
*)
val judge_intersect: body_a:RigidBodyInfo.t -> body_b:RigidBodyInfo.t -> separating_axis option
