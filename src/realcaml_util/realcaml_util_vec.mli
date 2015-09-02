
module Types = Realcaml_util_types

val empty : Types.vec
(* [empty] get the Zero vector for Vector of Three element *)

val four_empty: Types.four_vec 
(* [four_empty] get the Zero vector for Vector of Four element *)

val to_three: Types.four_vec -> Types.vec 
(* [to_three v] get converted three element vector from [v] *)

val to_four : Types.vec -> Types.four_vec
(* [to_four v] get converted four element vector from [v]. Value of element at four of vector is always [1.0]. *)
