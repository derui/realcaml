(**
   Providing contact information between two rigid body, and infomation of contact point.
   Provided contact information can have max four contact point.

   @version 0.1
   @author derui
*)

type t

(** Make contact data containing given arguements.  *)
val make: num:int -> friction:float -> points:ContactPoint.t array -> t

(** Get empty contact. *)
val empty: t

(** Get currect contact point information.  *)
val contact_num: t -> int

(** Get friction between contacted rigid bodies *)

val friction: t -> float

(** Get contact points contained given contact information. *)
val contact_points: t -> ContactPoint.t array

(** Set new contact points to the Contact and return new Contact *)
val set_contact_points: t -> ContactPoint.t array -> t
