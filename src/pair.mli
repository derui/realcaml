(**
   A Pair has pair containing rigid bodies each conflicted and infomation of contact them.

   @version 0.1
   @author derui
*)

(** Type of pair creation and keeping.  *)
type pair = New | Keep | Empty

type t = {
  pair_type:pair;
  key:int64;
  contact:Contact.t;
  indexA:int32; indexB:int32;
}

(** Make pair data by key, or two indecies.
    To make pair used two indecies equals given key,
    then indexA is first 32bits and indexB is second.
*)
val make_by_key: pt:pair -> key:int64 -> ?contact:Contact.t -> unit -> t
val make_by_index: pt:pair -> indexA:int32 -> indexB:int32 -> ?contact:Contact.t -> unit -> t

(** Get empty pair  *)
val empty: t
