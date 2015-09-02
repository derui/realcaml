(**
   Setup is parameters to setup constraint.

   @version 0.2
   @author derui
*)

module U = Realcaml_util

type t = {
  k : U.mat;
    (** The k matrix that is calculated to physical quantity of constraint *)
  restriction : float;
    (** The restriction between two bodies to calculate constraint *)
  relative_velocity : U.vec;
    (** relative velocity between two bodies. *)
  contact_bias : float;
    (** The bias for distance to detect bodies are not contact *)
  distance: float;
    (** The distance of contacted point between two bodies. *)
  time_step : float;
  (** The time step from [!Engine_option]. *)
}

