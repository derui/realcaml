(**
   The Prelude module.

   Some useful and basically functions contained this module.
   All functions doesn't belong any module, are used to some situation
   as utility.
*)

(** identity function *)
val id : 'a -> 'a
(** constant function. always return first argument. *)
val const : 'a -> 'b -> 'a
(** flip argument align. *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** control direction of to apply function to argument.
   |> looks like monad bind (>>=) is reversed to apply argument of
   normal direction.
*)
val (|>) : 'a -> ('a -> 'b ) -> 'b

(** control direction of to apply function to argument.
   |< is alias of normal function application.
*)
val (|<) : ('a -> 'b) -> 'a -> 'b

(** Combine two functions like from haskell's *)
val (@<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Combine two functions which direction to apply is forward *)
val (@>) : ('a -> 'b)  -> ('b -> 'c) -> 'a -> 'c


(** comparing floats that if one float has difference lesser than
  `epsilon`, result of comparing is `0` as true.
*)
val cmp_float : epsilon:float -> float -> float -> int

(** curry converts an uncurried function to a curried function *)
val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** convertes an curried function to an uncirried function *)
val uncurry: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
