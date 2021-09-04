(** Providing contact information between two rigid body, and infomation of contact point. Provided contact information
    can have max four contact point.

    @version 0.1
    @author derui *)

type t = {
  contact_num : int;  (** Number of the having contact points *)
  friction : float;  (** friction per contained contact points *)
  contact_points : Contact_point.t list;  (** contact points of this *)
}

val empty : t
(** Get empty contact. *)

val update_contact_points :
  body_a:Realcaml_rigid_body.Rigid_body_info.t ->
  body_b:Realcaml_rigid_body.Rigid_body_info.t ->
  closest:Closest_point.t ->
  t ->
  t
(** Get new contact structure updated with two bodies and a new closest point *)
