(* Stringable Type. *)
module type Type =
sig
  type t
  (* translate t into string. *)
  val to_string : t -> string
end

(* Signature of Stringable. *)
module type S =
sig
  type t
  (* printing type with Format.fomatter. *)
  val print : Format.formatter -> t -> unit
  (* type convert to character array. *)
  val to_chararray : t -> char array
  (* type conver to character list. *)
  val to_charlist : t -> char list
end

(* Functor to make new Stringable module. *)
module Make (T : Type) : S with type t := T.t
