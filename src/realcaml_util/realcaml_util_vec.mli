
val empty : unit -> Realcaml_util_types.vec
(* [empty] get the Zero vector for Vector of Three element *)

val four_empty: unit -> Realcaml_util_types.four_vec 
(* [four_empty] get the Zero vector for Vector of Four element *)

val to_three: Realcaml_util_types.four_vec -> Realcaml_util_types.vec 
(* [to_three v] get converted three element vector from [v] *)

val to_four : Realcaml_util_types.vec -> Realcaml_util_types.four_vec
(* [to_four v] get converted four element vector from [v]. Value of element at four of vector is always [1.0]. *)
