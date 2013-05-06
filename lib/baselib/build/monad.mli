open Monad_intf

(**
   Make module-specific module of T with module-specific module
   implemented of Type.

   If you need binary operator to use without module-prefix, call
   X.Open. (X is your module maked by this functor)
   X.Open extract only binary operators. other functions are not extracted.
*)
module Make (A : Type) : S with type 'a t := 'a A.t
module Make2 (A : Type2) : S2 with type ('a, 'z) t := ('a, 'z) A.t
