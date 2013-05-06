(** This module define [Map] module for [Baselib.Std]. I named after {!Ext_map}
    because don't override Original Caml {!Map} module.

    Internal structure defined this module is based on 'red-black tree',
    so all operation to defined Map by this module are O(log n) at worst.

    All defined operations has label-interface and first-container arguments that
    is locate first in operation's arguments.

    Usage:

    1. want map to use Pervasive.compare
         type map = ('a ,'b) Baselib.Std.Map.Poly.t
       or
         module M = Baselib.Std.Map.Poly
    2. want map with using own comparable module.
         module M = Baselib.Std.Map.Make(Type)
    3. want module interface to write for .mli
         module M : Baselib.Std.Map.S with type Key.t = {b key}
       in ml,
         module M = Baselib.Std.Map.Make(struct
           type t = {b key}
           let compare = ...
         end)
       // Using key you want is as {b key}.

    @author derui
    @since 0.1
*)

open Ext_map_intf

(** type for generic map  *)
type ('k, +'v, 'comparator) t
type 'k key = 'k
type ('a, 'comparator, 'z) create = ('a, 'comparator, 'z) create_options_with_comparator

include Creator with type ('k, 'v, 'comparator) t := ('k, 'v, 'comparator) t
                and type 'k key := 'k key
                and type ('a, 'comparator, 'z) create := ('a, 'comparator, 'z) create

include Accessor with type ('k, 'v, 'c) t := ('k, 'v, 'c) t
                 and type 'k key := 'k key


module type Key = Key
(** Used by to make key-specified {!Map} module by {!Make} *)
module type S = S with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

(** Make key-specified module with functor implemented
    compare between a key and define the type of key
*)
module Make(Key:Key) : S with type Key.t = Key.t

module Make_use_comparator(Key:Comparable.S) : S
  with type Key.t = Key.t
  and type Key.comparator = Key.comparator

module Poly : sig
  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type ('a, 'b) t = ('a, 'b, Comparable.Poly.comparator) map
  type ('a, 'b, 'c) t_ = ('a, 'b) t
  type 'a key = 'a
  type ('a, 'b, 'c) create = ('a, 'b, 'c) create_options_without_comparator

  include Creator with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
                  and type 'a key := 'a key
                  and type ('a, 'b, 'c) create := ('a, 'b, 'c) create

  include Accessor with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
                   and type 'a key := 'a key
end
  with type ('a, 'b, 'c) map = ('a, 'b, 'c) t
