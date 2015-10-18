
val empty : unit -> Realcaml_util_types.vec
(* [empty] get the Zero vector for Vector of Three element *)

val unit: [<`X|`Y|`Z] -> Realcaml_util_types.vec
(* [unit `X] get the unit vector on the x, y or z axis. *)

val four_empty: unit -> Realcaml_util_types.four_vec 
(* [four_empty] get the Zero vector for Vector of Four element *)

val to_three: Realcaml_util_types.four_vec -> Realcaml_util_types.vec 
(* [to_three v] get converted three element vector from [v] *)

val to_four : Realcaml_util_types.vec -> Realcaml_util_types.four_vec
(* [to_four v] get converted four element vector from [v]. Value of element at four of vector is always [1.0]. *)

val pos_to_vec: x:float -> y:float -> z:float -> unit -> Realcaml_util_types.vec
(* [pos_to_vec ~x ~y ~z ()] get the vector expressed a position in 3D. *)

val extract_three: Realcaml_util_types.vec -> float * float * float
(* [extract_three v] get values as x/y/z axis in the [vec]. *)

val with_four: Realcaml_util_types.vec -> f:(Realcaml_util_types.four_vec -> Realcaml_util_types.four_vec) -> Realcaml_util_types.vec
(* [with_four v ~f] execute [~f] with converted four dimensions vector from [v], and convert three dimension vector from returned [~f]. *)
