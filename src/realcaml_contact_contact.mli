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
  contact_points:Realcaml_contact_contact_point.t list;
}

(** Get empty contact. *)
val empty: t

(** Get new contact structure updated with two bodies and a new closest point *)
val update_contact_points : body_a:Realcaml_rigid_body.Rigid_body_info.t -> body_b:Realcaml_rigid_body.Rigid_body_info.t ->
  closest:Realcaml_contact_closest_point.t -> t -> t
