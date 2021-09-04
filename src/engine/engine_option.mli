type t = {
  (* [time_step] is interval of each calculation step. *)
  time_step : float;
  (* [contact_bias] is used to solve constraints, to allow contact rigid bodies. *)
  contact_bias : float;
  (* [contact_stop] is used to solve constraints, to stop solving constrait. *)
  contact_stop : float;
  (* [iteration] is count to iterate to solve constraint on each time step. *)
  iteration : int;
  (* [max_bodies] specifies count to be able to manage rigid body *)
  max_bodies : int;
  (* [max_pairs] specifies count to be able to manage contacts. *)
  max_pairs : int;
  (* [gravity] is axis for gravity. *)
  gravity : Realcaml_util.Types.vec;
}
(** option for engine *)

val empty : unit -> t
(** [empty ()] gets the default option. *)
