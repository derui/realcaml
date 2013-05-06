(* Stringable Type. *)
module type Type =
sig
  type 'a t
  (* translate t into string. *)
  val to_string : 'a t -> string
end

(* Signature of Stringable. *)
module type S =
sig
  type 'a t
  (* printing type with Format.fomatter. *)
  val print : Format.formatter -> 'a t -> unit
  (* type convert to character array. *)
  val to_chararray : 'a t -> char array
  (* type conver to character list. *)
  val to_charlist : 'a t -> char list
end

(* Functor to make new Stringable module. *)
module Make (T : Type) : S with type 'a t := 'a T.t
