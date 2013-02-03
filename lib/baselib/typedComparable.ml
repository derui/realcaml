
module type Type =
sig
  type 'a t
  val compare : 'a t -> 'a t -> int
end

module type S =
sig
  type 'a t

  val eq : 'a t -> 'a t -> bool
  val ne : 'a t -> 'a t -> bool
  val gt : 'a t -> 'a t -> bool
  val ge : 'a t -> 'a t -> bool
  val lt : 'a t -> 'a t -> bool
  val le : 'a t -> 'a t -> bool
end

(* using this module when need order functions. *)
module Make (T : Type) : S with type 'a t := 'a T.t =
struct
  open T

  let eq x y = compare x y = 0
  let ne x y = compare x y <> 0
  let gt x y = compare x y > 0
  let ge x y = compare x y >= 0
  let lt x y = compare x y < 0
  let le x y = compare x y <= 0
end
