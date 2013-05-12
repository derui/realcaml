(**
   A Engine module executes physics simulation with many rigid bodies.
   Executing simulation in this Engine is named `simulation pipeline` which is
   consist of Broad phase, narrow phase, solve constraints and integrating
   position of rigid bodies.

   @version 0.1
   @author derui
*)

(** type of engine  *)
type t

val make: ?time_step:float -> ?contact_bias:float -> ?contact_stop:float ->
  ?iteration:int -> ?max_bodies:int -> ?max_pairs:int -> unit -> t
(** [make ?time_step ?contact_bias ?contact_stop ?iteration ?max_bodies ?max_pairs ()] give
    you engine is made with given parameters.
    [time_step] is time of per step that equals frame. [contact_bias] and [contact_stop] is
    used to solve constratints, maybe not need to change value from default.
    [iteration] specifies iteration count to solve constratints.
    [max_bodies] and [max_pairs] specifies to be able to manage rigid body and contact informations.

    @param ?time_step [0.016]
    @param ?contact_bias [0.1]
    @param ?contact_stop [0.001]
    @param ?iteration [10]
    @param ?max_bodies [500]
    @param ?max_pairs [5000]
*)

val execute_pipeline : t -> t
(** Run simulation pipeline by given engine. Executing result is effected to given engine. *)
