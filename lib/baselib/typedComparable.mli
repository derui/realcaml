
(* this module is used to functor when needs some modules to compare t
   with type variable.  *)
module type Type =
sig
  type 'a t
  (* compare elements. if first @t@ is less than second, return -1.
     if first @t@ equal second, return 0. 
     return 1 if first @t@ is greater than second.
  *)
  val compare : 'a t -> 'a t -> int
end

module type S =
sig
  (* element type of this signature. this is had to overwrite when
     using any module. *)
  type 'a t

  (* equals *)
  val eq : 'a t -> 'a t -> bool
  (* no'a t equals *)
  val ne : 'a t -> 'a t -> bool
  (* greater than *)
  val gt : 'a t -> 'a t -> bool
  (* greater equal *)
  val ge : 'a t -> 'a t -> bool
  (* less than *)
  val lt : 'a t -> 'a t -> bool
  (* less equal *)
  val le : 'a t -> 'a t -> bool
end

(* using this module when need order functions. *)
module Make (T : Type) : S with type 'a t := 'a T.t
