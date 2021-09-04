val empty : unit -> Types.vec
(* [empty] get the Zero vector for Vector of Three element *)

val unit : [< `X | `Y | `Z ] -> Types.vec
(* [unit `X] get the unit vector on the x, y or z axis. *)

val four_empty : unit -> Types.four_vec
(* [four_empty] get the Zero vector for Vector of Four element *)

val to_three : Types.four_vec -> Types.vec
(* [to_three v] get converted three element vector from [v] *)

val to_four : Types.vec -> Types.four_vec
(* [to_four v] get converted four element vector from [v]. Value of element at four of vector is always [1.0]. *)

val pos_to_vec : x:float -> y:float -> z:float -> unit -> Types.vec
(* [pos_to_vec ~x ~y ~z ()] get the vector expressed a position in 3D. *)

val extract_three : Types.vec -> float * float * float
(* [extract_three v] get values as x/y/z axis in the [vec]. *)

val with_four : Types.vec -> f:(Types.four_vec -> Types.four_vec) -> Types.vec
(* [with_four v ~f] execute [~f] with converted four dimensions vector from [v], and convert three dimension vector from
   returned [~f]. *)
