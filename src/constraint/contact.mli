(**
   Providing contact information between two rigid body, and infomation of contact point.
   Provided contact information can have max four contact point.

   @version 0.1
   @author derui
*)

module R = Realcaml_rigid_body

type t = {
  (** Number of the having contact points *)
  contact_num:int;
  (** friction per contained contact points  *)
  friction:float;
  (** contact points of this *)
  contact_points:Contact_point.t list;
}

(** Get empty contact. *)
val empty: t

(** Get new contact structure updated with two bodies and a new closest point *)
val update_contact_points : body_a:R.Rigid_body_info.t -> body_b:R.Rigid_body_info.t ->
  closest:ClosestPoint.t -> t -> t
