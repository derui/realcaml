(**
   Setup is parameters to setup constraint.

   @version 0.2
   @author derui
*)

type t = {
  k : Types.mat;
    (** The k matrix that is calculated to physical quantity of constraint *)
  restriction : float;
    (** The restriction between two bodies to calculate constraint *)
  relative_velocity : Types.vec;
    (** relative velocity between two bodies. *)
  contact_bias : float;
    (** The bias for distance to detect bodies are not contact *)
  distance: float;
    (** The distance of contacted point between two bodies. *)
  time_step : float;
  (** The time step from [!Engine_option]. *)
}

