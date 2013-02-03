(**
   A Pair has pair containing rigid bodies each conflicted and infomation of contact them.

   @version 0.1
   @author derui
*)

(** Type of pair creation and keeping.  *)
type pair = New | Keep

type t

(** Make pair data by key, or two indecies.
    To make pair used two indecies equals given key,
    then indexA is first 32bits and indexB is second.
*)
val make_by_key: pt:pair -> key:int64 -> ?contact:Contact.t -> unit -> t
val make_by_index: pt:pair -> indexA:int32 -> indexB:int32 -> ?contact:Contact.t -> unit -> t

(** Get empty pair  *)
val empty: t

(** Get type current status of pair creation or keeping   *)
val pair_type : t -> pair

(** Get unique key of Pair that is conbined indexOfA and indexOfB.
    The first 32bit data of returned key is indexOfA, and second 32bit is
    indexOfB.
*)
val key: t -> int64

(** Get index of the Rigit body A. *)
val indexOfA: t -> int32

(** Get index of the Rigit body B. *)
val indexOfB: t -> int32

(** Get contact information which is between two rigid bodies.  *)
val contact : t -> Contact.t

(** Update contact information of the pair *)
val update_contact: t -> Contact.t -> t

(** Update pair type of given pair.  *)
val change_pair: t -> pair -> t
