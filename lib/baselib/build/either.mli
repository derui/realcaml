(** Provide culculation have two way result with type and relative
    functions for it.
    And Either providing monadic interface as {!Monad} module.

    Either Monad woking as follows:

    | Left  ->  finishing bind chain and return as Left
    | Right ->  continue bind chain applied function to Right value

    @author derui
    @since 0.1
*)

(** Have two way result. Often Left is failed to culculate and
    error message, Right as `right' calculation with calculated
    result.

    Note: Please look cautious type variant of {Either.t} before  use
    it. Sorry the variant arrangements is not very understand that
    the first type variant is for {b Right}, and second it is for {b
    Left}. Reason of this design is to add monadic interface to
    Either. (Added monadic interface here detail locate in {!monad} module.)
*)
type ('a, 'b) t =
| Left of 'b
| Right of 'a

(**
   If given {!either} is left, return applied {!f} to Left value.
   Return None if given {!either} is Right.
*)
val either_left : ('a, 'b) t -> ('b -> 'c) -> 'c option

(**
   If given {!either} is Right, return applied {!f} to Right value.
   Return None if given {!either} is Left.
*)
val either_right : ('a, 'b) t -> ('a -> 'c) -> 'c option

(**
   Return the given {!either} is {!Left} or not.
*)
val is_left : ('a, 'b) t -> bool

(**
   Return the given {!either} is {!Right} or not.
*)
val is_right : ('a, 'b) t -> bool

(** For Monadic operations *)
include Monad_intf.S2 with type ('a, 'b) t := ('a, 'b) t
