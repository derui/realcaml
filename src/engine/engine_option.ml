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

let gravity_axis =
  let module V = Typedvec.Algebra.Vec in
  let v = Realcaml_util.Vec.empty () in
  V.set v ~index:0 ~v:0.0;
  V.set v ~index:1 ~v:9.8;
  V.set v ~index:2 ~v:0.0;
  v

let empty () =
  {
    time_step = 0.016;
    contact_bias = 0.1;
    contact_stop = 0.001;
    iteration = 10;
    max_bodies = 500;
    max_pairs = 5000;
    gravity = gravity_axis;
  }
