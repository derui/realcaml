(**
   Basic interface for module convert to monadic module.
   Implementation along this interface for module is user responsibility.
*)
module type Type = sig
  (** type of monadic module.
      Have only one type variant type.
  *)
  type +'a t

  (** Return wrapped value as monad. *)
  val return : 'a -> 'a t

  (** binding monad with function, and return new monad having other type. *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Integrating module for binary operators.
    All binary operators containing this module are contained of
    {!S} module in the same way.
*)
module type Open = sig
  type +'a t

  (** Synonym for bind *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(**
   Additional to more function using basic function provided from
   Type.
   This module have implementation, so you don't need to implement this.
*)
module type S = sig

  include Open
  module Open : Open with type 'a t = 'a t

  (** alias {!bind} on {!Type} *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** given unmonadic function apply to monad.
      So-called `lift' other instruction.
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Return monadic list that consist of getting value from each
      monad *)
  val sequence : 'a t list -> 'a list t
end

(**
   Basic interface for module convert to monadic module.
   Modules that have -2 suffix in module name affects
   [only first type variant], be careful!

   Implementation along this interface for module is user responsibility.
*)
module type Type2 = sig
  (** type of monadic module for having 2 type variant.
  *)
  type ('a, 'z) t

  (** Return wrapped value as monad. *)
  val return : 'a -> ('a, 'z) t

  (** binding monad with function, and return new monad having other type. *)
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
end

(** Integrating module for binary operators.
    All binary operators containing this module are contained of
    {!S2} module in the same way.
*)
module type Open2 = sig
  type ('a, 'z) t
  (** synonym for bind *)
  val (>>=) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
  val return : 'a -> ('a, 'z) t

  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
end

(**
   Additional to more function using basic function provided from
   Type.
   This module have implementation, so you don't need to implement this.
*)
module type S2 = sig
  include Open2
  module Open : Open2 with type ('a, 'z) t = ('a, 'z) t

  (** given unmonadic function apply to monad.
      So-called `lift' other instruction.
  *)
  val map : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t

  (** Return monadic list that consist of getting value from each
      monad *)
  val sequence : ('a, 'z) t list -> ('a list, 'z) t
end
