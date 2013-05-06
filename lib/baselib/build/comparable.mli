(** this module is used to functor when needs some modules to compare
    t.

    usage: when you want to add compareable ability into module,
    include with applying Make that is in this module.

    include Compareable.Make(...)
*)

type ('a, 'unique_id) t = ('a -> 'a -> int)

(** avoid circular reference, only using inner this module.  *)
type ('a, 'unique_id) t_ = ('a, 'unique_id) t

(** Provide type-specific function to compare  *)
module type Type =
sig
  type t
  (** compare elements. if first is less than second, return -1.
      if first equal second, return 0.
      return 1 if first is greater than second.
  *)
  val compare : t -> t -> int
end

(** Signature of some functions to compare each specified type *)
module type S =
sig
  type t
  type comparator
  val comparator: (t, comparator) t_
  (** equals between elements *)
  val eq : t -> t -> bool
  (** not equals between elements *)
  val ne : t -> t -> bool
  (** first element is greater than second *)
  val gt : t -> t -> bool
  (** first element is greater equal second *)
  val ge : t -> t -> bool
  (** first element is less than second *)
  val lt : t -> t -> bool
  (** first element is less equal second *)
  val le : t -> t -> bool

end

(** Signature of some functions to compare to have a variable type. *)
module type S1 =
sig
  (** element type of this signature. this is had to overwrite when
      using any module. *)
  type 'a t
  type comparator
  val comparator: ('a t, comparator) t_
  (** equals between elements *)
  val eq : 'a t -> 'a t -> bool
  (** not equals between elements *)
  val ne : 'a t -> 'a t -> bool
  (** first element is greater than second *)
  val gt : 'a t -> 'a t -> bool
  (** first element is greater equal second *)
  val ge : 'a t -> 'a t -> bool
  (** first element is less than second *)
  val lt : 'a t -> 'a t -> bool
  (** first element is less equal second *)
  val le : 'a t -> 'a t -> bool

end

(** comparable module for primitive types that
    is able to apply polymorphismical Perversive's compare to it.
*)
module Poly : S1 with type 'a t = 'a

(** using this module when need order functions. *)
module Make (T : Type) : S with type t = T.t

(** Use this module to get type-specified comparable module  *)
module Make1 (T : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
end) : S1 with type 'a t = 'a T.t

(** Convert {!S} to {!S1} *)
module S_to_S1 (T: S) : S1 with type 'a t = T.t
                           and type comparator = T.comparator
