(**
   This module is provided to apply force with rigid body. Force and torque to be able to apply
   Rigid Body are  implements in this.

   @author derui
   @version 0.2
*)

(**
   Apply force to body and state with given force and torque.
*)
val apply_force  : body:RigidBody.t -> state:State.t ->
  force:Candyvec.Vector.t -> torque:Candyvec.Vector.t -> time_step:float -> RigidBody.t * State.t
