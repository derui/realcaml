(** add ability that can compare and convert module.t to string *)

(** Interface for Functor to apply to Make module  *)
module type Type =
  sig
    (** add compareable ability between same module.t  *)
    include Comparable.Type
    (** add stringable ability that can convert module.t to string  *)
    include Stringable.Type with type t := t
  end

(** Module implementation interface  *)
module type S =
  sig
    include Comparable.S
    include Stringable.S with type t := t
  end

(**
   Make module that add compareable and stringable ability to
   target module.
   Prepareing Functor that implemented to Type before make new added
   module!
*)
module Make (T:Type) : S with type t := T.t
