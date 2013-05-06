(**
   Add some functions for predefined {!option}.
   this module provide below modules for 'a option.

   - Monad
   - TypedCollection

   Use Make functor if you need type-specific option.
   But, have remaks of specifiied module made by Make,
   it can't use as Monad, so if need monadic Option, you use no
   specified option, that is this module, which is solo access.
*)

(** Convert Option to string with translating function. *)
val to_string : ('a -> string) -> 'a option -> string

(** output string that converted option. *)
val print : Format.formatter -> ('a -> string) -> 'a option -> unit

(** return true if given option is None. *)
val is_none : 'a option -> bool

(** return true if given option is Some and some value. *)
val is_some : 'a option -> bool

(** Compared to between two options with comparator for
    between type variant of type.

    Comparing resulted by this function are as follows:

    | fst  | snd    | result
    | -----+--------+--------
    | None | None   |   0
    | Some | None   |   1
    | None | Some   |  -1
    | Some | Some   | depend on comparator

    comparator result are expected to equal {!Pervasive.compare}.
    @param comparator compared to between any type value. default is {!Pervasive.compare}
    @param first A option value
    @param second A option value
    @return -1 is lesser than, 1 is greater than, to equal 1
*)
val compare_with : ?comparator:('a -> 'a -> int) -> 'a option -> 'a option -> int

(** this function applies given function to each element of list,
    and builds the list with the results returned by given function.
    But, appling given function only target Some variant. None is ignored.
*)
val map : ('a -> 'b) -> 'a option list -> 'b list

(**
   Provide monadic operation with 'a option.

   Monadic option behaved under {!Monad.Type} functions are following:

   | return : Wraping given argument to Some.
   | bind   : If given monad is None, chain of bindings finish on the
   |          spot, and return None.
   |          Otherwise it apply function and return result of
   |          function wrapped Some
*)
include Monad_intf.S with type 'a t := 'a option
include TypedCollection.Type with type 'a t := 'a option
include TypedCollection.S with type 'a t := 'a option
