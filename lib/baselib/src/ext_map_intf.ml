(** Define key type and how to compare.
*)
module type Key = Comparable.Type

(** Define some operations that are access functions, for instance,
    add, remove, find, map, iter, and so on.

    This module can use all {!Ext_map} and specialized map created by {!Make} module.
*)
module type Accessor = sig

  (** type of key.  *)
  type 'k key
  (** type of this map with comparator  *)
  type ('k, 'v, 'c) t

  (** Return given map is wheter empty or not   *)
  val is_empty: (_, _, _) t -> bool

  (** Add data to given Map and return to result in added data  *)
  val add: ('k, 'v, 'c) t -> key:'k key -> data:'v -> ('k, 'v, 'c) t

  (** Remove key and related data from given Map, and return removed Map *)
  val remove: ('k, 'v, 'c) t -> 'k key -> ('k, 'v, 'c) t

  (** Get minimum key-value pair, if empty Map is given, return None.
      when use *_exn, raise {!Invalid_argument} if empty tree is given.
  *)
  val minimum: ('k, 'v, 'c) t -> ('k key * 'v) option
  val minimum_exn: ('k, 'v, 'c) t -> 'k key * 'v

  (** Get maximum key-value pair, if empty Map is given, return None.
      when use *_exn, raise {!Invalid_argument} if empty tree is given.
  *)
  val maximum: ('k, 'v, 'c) t -> ('k key * 'v) option
  val maximum_exn: ('k, 'v, 'c) t -> 'k key * 'v

  (** Search element is related to given key, *_exn raise {!Not_found} if
      key not found.
  *)
  val find: ('k, 'v, 'c) t -> 'k key -> 'v option
  val find_exn: ('k, 'v, 'c) t -> 'k key -> 'v

  (** Return given key whether contains in container or not *)
  val mem: ('k, 'v, 'c) t -> 'k key -> bool

  (** Apply function to each associated pair. To use when function have only side-effect.  *)
  val iter: ('k, 'v, 'c) t -> ('v -> unit) -> unit
  val iteri: ('k, 'v, 'c) t -> (key:'k key -> data:'v -> unit) -> unit

  (** Apply function to each associated pair, and return new Map consist of result in to apply function. *)
  val map: ('k, 'v, 'c) t -> ('v -> 'x) -> ('k, 'x, 'c) t

  (** Apply function to each associated pair, and return new Map consist of result in to apply function.
      Difference of {!Accessor.map} is that key related data add to argument given function.
  *)
  val mapi: ('k, 'v, 'c) t -> (key:'k key -> data:'v -> 'x) -> ('k, 'x, 'c) t

  (** Folds over keys and data in map in ascending order.  *)
  val fold: ('k, 'v, _) t -> f:(key:'k key -> data:'v -> 'a -> 'a) -> init:'a -> 'a

  (** Total ordering between maps. The first argument is a total ordering used to
      compare data associated with equal keys in the two maps.  *)
  val compare: ('v -> 'v -> int) -> ('k, 'v, 'c) t -> ('k, 'v, 'c) t -> int

  (** equal cmp m1 m2 tests whether the maps m1 and m2 are equal, that is,
      contain equal keys and associate them with equal data. cmp is the equality
      predicate used to compare the data associated with the keys.
  *)
  val equal: ('v -> 'v -> bool) -> ('k, 'v, 'c) t -> ('k, 'v, 'c) t -> bool

  (** returns list of keys in map *)
  val keys: ('k, _, _) t -> 'k key list

  (** returns list of data in map  *)
  val data: (_, 'v, _) t -> 'v list
end

(** When create map by {!Creator} with comparator, use {!create_options_with_comparator}.
    But don't need to use comparator given type parameter,
    use {!create_options_without_comparator}.
*)
type ('a, 'comparator, 'z) create_options_without_comparator = 'z
type ('a, 'comparator, 'z) create_options_with_comparator =
    comparator:('a, 'comparator) Comparable.t -> 'z

(** contains some ways to create map. *)
module type Creator = sig

  (** type of map to create or conversion  *)
  type 'k key
  type ('k, 'v, 'comparator) t
  type ('a, 'comparator, 'z) create

  (** Empty map that is used to initial map for some operations *)
  val empty : ('a, 'comparator, ('a, _, 'comparator) t) create

  (** map with single pair, value and key  *)
  val singleton: ('a, 'comparator, 'a key -> 'v -> ('a, 'v, 'comparator) t) create

  (** convert Map to key-value alist.
      if given map include duplicated key, raise exception

      TODO: Being customizable include duplicate key in map...
  *)
  val of_alist: ('a, 'comparator, ('a, 'v, 'comparator) t -> ('a key * 'v) list) create
end

(** This is signature to make type-specified Map module. *)
module type S = sig
  (** module which has type of the key of this module *)
  module Key : Comparable.S

  (** type of implementation of map *)
  type ('k, +'v, 'c) map
  (** type of key specified map  *)
  type +'v t = (Key.t, 'v, Key.comparator) map

  (** two type variant tied key-specified type  *)
  type ('k, 'v, 'c) t_ = 'v t
  (** type for key specified *)
  type key = Key.t
  type 'k key_ = key
  type ('a, 'b, 'c) create = ('a, 'b, 'c) create_options_without_comparator

  (** inherit {!Accessor} module  *)
  include Accessor with type ('k, 'v, 'c) t := ('k, 'v, 'c) t_
                   and type 'k key := 'k key_

  (** inherit {!Creator} module *)
  include Creator with type ('k, 'v, 'c) t := ('k, 'v, 'c) t_
                  and type 'k key := 'k key_
                  and type ('a, 'b, 'c) create := ('a, 'b, 'c) create

end
