type ('a, 'unique_id) t = ('a -> 'a -> int)

(** avoid circular reference  *)
type ('a, 'unique_id) t_ = ('a, 'unique_id) t


module type Type =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type t
  type comparator
  val comparator: (t, comparator) t_
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool
  val lt : t -> t -> bool
  val le : t -> t -> bool
end

module type S1 =
sig
  type 'a t
  type comparator
  val comparator: ('a t, comparator) t_
  val eq : 'a t -> 'a t -> bool
  val ne : 'a t -> 'a t -> bool
  val gt : 'a t -> 'a t -> bool
  val ge : 'a t -> 'a t -> bool
  val lt : 'a t -> 'a t -> bool
  val le : 'a t -> 'a t -> bool
end

module Make (T : Type) = struct
  type t = T.t
  type comparator
  let comparator = T.compare
  let eq t1 t2 = compare t1 t2 = 0
  let ne t1 t2 = compare t1 t2 <> 0
  let gt t1 t2 = compare t1 t2 > 0
  let ge t1 t2 = compare t1 t2 >= 0
  let lt t1 t2 = compare t1 t2 < 0
  let le t1 t2 = compare t1 t2 <= 0
end

module Make1 (T : sig
  type 'a t
  val compare: 'a t -> 'a t -> int
end) = struct
  type 'a t = 'a T.t
  type comparator
  let comparator = T.compare
  let eq t1 t2 = compare t1 t2 = 0
  let ne t1 t2 = compare t1 t2 <> 0
  let gt t1 t2 = compare t1 t2 > 0
  let ge t1 t2 = compare t1 t2 >= 0
  let lt t1 t2 = compare t1 t2 < 0
  let le t1 t2 = compare t1 t2 <= 0
end

module Poly = struct
  include Make1 (struct
    type 'a t = 'a
    let compare = Pervasives.compare
  end)
end

module S_to_S1 (T : S) = struct
  type 'a t = T.t
  type comparator = T.comparator
  let comparator = T.comparator

  let eq = T.eq
  let ne = T.ne
  let gt = T.gt
  let ge = T.ge
  let lt = T.lt
  let le = T.le
end
