(** option for engine  *)
type engine_option = {
  time_step:float;
  contact_bias:float;
  contact_stop:float;
  iteration:int;
  max_bodies:int;
  max_pairs:int;
}
