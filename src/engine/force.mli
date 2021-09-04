(**
   This module is provided to apply force with rigid body. Force and torque to be able to apply
   Rigid Body are  implements in this.

   @author derui
   @version 0.2
*)

(**
   Apply force to body and state with given force and torque.
*)
val apply_force  :
  body:Realcaml_rigid_body.Rigid_body.t ->
  state:Realcaml_rigid_body.State.t ->
  force:Realcaml_util.vec ->
  torque:Realcaml_util.vec ->
  time_step:float ->
  unit -> Realcaml_rigid_body.Rigid_body.t * Realcaml_rigid_body.State.t
