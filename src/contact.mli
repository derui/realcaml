(**
   Providing contact information between two rigid body, and infomation of contact point.
   Provided contact information can have max four contact point.

   @version 0.1
   @author derui
*)

type t = {
  (** Number of the having contact points *)
  contact_num:int;
  (** friction per contained contact points  *)
  friction:float;
  (** contact points of this *)
  contact_points:ContactPoint.t list;
}

(** Get empty contact. *)
val empty: t

(** Get new contact structure updated with two bodies and a new closest point *)
val update_contact_points : contact:t -> body_a:RigidBodyInfo.t -> body_b:RigidBodyInfo.t ->
  closest:(Candyvec.Std.Vector.t * float) -> t
