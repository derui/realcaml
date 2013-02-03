open Ext_map_intf

(* Based on "AA tree".
   But changes direction to fixup function perform that is to root-to-leaf,
   so minimum cost for calculation is more than basic theory and implements.
*)
module Tree = struct

  (* color of node and leaf *)
  type color = Red | Black

  (* element of tree.
     Empty is leaf as watcher, so virtual root and leaf.
  *)
  type ('k, 'v) t =
    | Empty                     (* always level 1 *)
    (* Node have level always greater than 1.  *)
    | Node of int *                         (* level of node *)
        ('k, 'v) t *                    (* left child *)
        'k *                            (* key of node *)
        'v *                          (* data of node *)
        ('k, 'v) t                  (* right child *)

  (* helper function *)
  let level = function
    | Empty -> 1
    | Node (l, _, _, _, _) -> l

  (* create empty root *)
  let empty = Empty

  let is_empty = function
    | Empty -> true
    | Node _ -> false

  (* create map single key with data. Node have always left and right Empty as leaf. *)
  let singleton k d = Node (2, Empty, k, d, Empty)

  (* find node from tree with target key.
     If node having key is not found, return None
  *)
  let rec search tree k compare_key =
    match tree with
    | Empty -> None
    | Node (_, l, nk, v, r) ->
      let c = compare_key k nk in
      if c = 0 then
        Some v
      else if c < 0 then
        search l k compare_key
      else
        search r k compare_key

  (* tests that if element having k is located in tree*)
  let exists tree k compare_key = Option.is_some (search tree k compare_key)

  (* balance horizontal left link *)
  let skew = function
    | Empty -> Empty
    (* Node have two child both of leaf *)
    | Node (_, Empty, _, _, Empty) as n -> n
    (* skew *)
    | Node (level, Node (llevel, ll, lk, ld, lr), k, d, r) as n ->
      if level = llevel then
        Node (llevel, ll, lk, ld, Node (level, lr, k, d, r))
      else
        n
    | _ as n -> n

  (* balance horizontal right-right link *)
  let split = function
    | Empty -> Empty
    (* Node have two child both of leaf *)
    | Node (_, Empty, _, _, Empty) as n -> n
    (* split *)
    | Node (lvl, l, k, d, Node (rlevel, rl, rk, rd, rr)) as n ->
      if lvl = level rr then
        Node (rlevel + 1, Node (lvl, l, k, d, rl), rk, rd, rr)
      else
        n
    | _ as n -> n

  (* fix unbalanced tree to balance tree *)
  let fixup_balance tree = split (skew tree)

  (* add key-value pair to given tree and return added tree. *)
  let add tree ~key ~data ~compare_key =
    let rec add_inner tree key data =
      match tree with
      | Empty -> singleton key data
      | Node (level, l, k, d, r) ->
        (* apply fixup_balance each separated tree *)
        let c = compare_key key k in
        if c < 0 then
          fixup_balance (Node (level, (add_inner l key data), k, d, r))
        else if c = 0 then
          fixup_balance (Node (level, l, key, data, r))
        else
          fixup_balance (Node (level, l, k, d, (add_inner r key data)))
    in
    add_inner tree key data

    (* convert tree to assoc-list ordered ascending *)
  let of_alist tree =
    let rec of_alist_sub tree lst =
      match tree with
      | Empty -> lst
      | Node (_, l, k, d, r) ->
        of_alist_sub r ((k, d) :: (of_alist_sub l lst))
    in
    List.rev (of_alist_sub tree [])

  (** Return value related minimum key in given tree. If empty tree is given,
      return None.
  *)
  let rec minimum = function
    | Empty -> raise (Invalid_argument "minimum don't accept Empty tree")
    | Node (_, Empty, k, d, r) as n -> n
    | Node (_, l, _, _, _) -> minimum l

  (** Return value related minimum key in given tree. If empty tree is given,
      return None.
  *)
  let rec maximum = function
    | Empty -> raise (Invalid_argument "maxmum don't accept Empty tree")
    | Node (_, _, k, _, Empty) as n -> n
    | Node (_, _, _, _, r) -> maximum r

  (* Return minimum key-value pair  *)
  let min_binding tree =
    match minimum tree with
    | Empty -> raise (Invalid_argument "min_binding don't accept Empty tree")
    | Node (_, _, k, d, _) -> (k, d)

  (* Return maximum key-value pair *)
  let max_binding tree =
    match maximum tree with
    | Empty -> raise (Invalid_argument "max_binding don't accept Empty tree")
    | Node (_, _, k, d, _) -> (k, d)


  (* return successor node of root node. If successor is nothing when
     root node don't have right child, return Empty
  *)
  let successor = function
    | Empty -> Empty
    | Node (_, _, _, _, Empty) -> Empty
    | Node (_, _, _, _, r) -> minimum r

  (* return predecessor node of root node. If predecessor is nothing when
     root node don't have left child, return Empty
  *)
  let predecessor = function
    | Empty -> Empty
    | Node (_, Empty, _, _, _) -> Empty
    | Node (_, l, _, _, _) -> maximum l

  (* decrease current level of node. The result of this function is
     level of root and right equivalence or right is equal to root level - 1.
  *)
  let decrease_level tree =
    match tree with
    | Empty -> Empty
    | Node (lvl, l, k, d, Empty) ->
      let should_be = (min (level l) (level Empty)) + 1 in
      Node (min lvl should_be, l, k, d, Empty)
    | Node (lvl, l, k, d, Node (rlvl, rl, rk, rd, rr)) ->
      let should_be = (min (level l) (level (Node (rlvl, rl, rk, rd, rr)))) + 1 in
      Node (min lvl should_be, l, k, d, Node (min rlvl should_be, rl, rk, rd, rr))

  let remove_fixup_balance tree =
    let skew_all_level tree =
      match tree with
      | Empty -> Empty
      | Node (lvl, l, k, d, Empty) -> Empty
      | Node (lvl, l, k, d, r) ->
        match r with
        | Empty -> Empty
        | Node (_, _, _, _, Empty) ->
          Node (lvl, l, k, d, skew r)
        | Node (_, _, _, _, Node _ ) as r ->
          begin match skew r with
          | Empty -> failwith "Ext_map.remove_fixup_balance: inconceivative pattern match"
          | Node (rlvl, rl, rk, rd, rr) ->
            Node (lvl, l, k, d, Node (rlvl, rl, rk, rd, skew rr))
          end in
    let split_all_level = function
      | Empty -> Empty
      | Node (lvl, l, k, d, Empty) as n ->
        split n
      | Node (lvl, l, k, d, r) as n ->
        begin match split n with
        | Empty -> failwith "Ext_map.remove_fixup.balance: inconceivative pattern match"
        | Node (lvl, l, k, d, r) ->
          Node (lvl, l, k, d, split r)
        end in
    match tree with
    | Empty -> Empty
    | Node _ ->
      split_all_level (skew_all_level tree)

  (** Remove a pair related key and value, then return removed tree.
      If key don't contain given tree, return tree without change.
  *)
  let rec remove tree key compare_key =
    let rec remove_ tree key compare_key =
      match tree with
      | Empty -> Empty
      | Node (_, l, k, d, r) ->
        let c = compare_key k key in
        if c < 0 then
          remove_ l key compare_key
        else if c > 0 then
          remove_ r key compare_key
        else
          match tree with
          | Empty -> failwith "don't reachpattern match"
          | Node (lvl, Empty, k, d, Empty) -> Empty
          | Node (lvl, Empty, k, d, Node _) as tree ->
            begin match successor tree with
            | Empty -> failwith "don't reach pattern match"
            | Node (llvl, ll, lk, ld, lr) ->
              Node (lvl, Empty, lk, ld, remove_ r key compare_key)
            end
          | Node (lvl, Node _, k, d, r) ->
            begin match predecessor tree with
            | Empty -> failwith "don't reach pattern match"
            | Node (llvl, ll, lk, ld, lr) ->
              Node (lvl, remove_ l lk compare_key, lk, ld, r)
            end in
    let removed = remove_ tree key compare_key in
    let removed = decrease_level removed in
    remove_fixup_balance removed

  let rec fold tree ~f ~init:accu =
    match tree with
    | Empty -> accu
    | Node (_, l, k, d, r) ->
      fold r ~f ~init:(f ~key:k ~data:d (fold l ~f ~init:accu))

  let rec map tree ~f ~compare_key =
    match tree with
    | Empty -> Empty
    | Node (lvl, l, k, d, r) ->
      let l = map l ~f ~compare_key in
      let d = f d in
      let r = map r ~f ~compare_key in
      Node (lvl, l, k, d, r)

  let rec mapi tree ~f ~compare_key =
    match tree with
    | Empty -> Empty
    | Node (lvl, l, k, d, r) ->
      let l = mapi l ~f ~compare_key in
      let d = f ~key:k ~data:d in
      let r = mapi r ~f ~compare_key in
      Node (lvl, l, k, d, r)

  let rec iter tree f compare_key =
    match tree with
    | Empty -> ()
    | Node (lvl, l, k, d, r) ->
      iter l f compare_key;
      f d;
      iter r f compare_key

  let rec iteri tree f compare_key =
    match tree with
    | Empty -> ()
    | Node (lvl, l, k, d, r) ->
      iteri l f compare_key;
      f ~key:k ~data:d;
      iteri r f compare_key

  let keys tree =
    fold tree ~f:(fun ~key ~data lst -> key :: lst) ~init:[]

  let data tree =
    fold tree ~f:(fun ~key ~data lst -> data :: lst) ~init:[]

  type ('a, 'b) enumeration = End | More of 'a * 'b * ('a, 'b) t * ('a, 'b) enumeration

  let rec cons_enum m e =
    match m with
    | Empty -> e
    | Node (lvl, l, v, d, r) -> cons_enum l (More (v, d, r, e))

  let compare f first second compare_key =
    let rec compare_aux first second =
      match (first, second) with
      | (End, End) -> 0
      | (_, End) -> (-1)
      | (End, _) -> 1
      | (More (k1, d1, r1, e1), More (k2, d2, r2, e2)) ->
        let c = compare_key k1 k2 in
        if c <> 0 then c else
          let c = f d1 d2 in
          if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e1)
    in compare_aux (cons_enum first End) (cons_enum second End)

  let equal f first second compare_key =
    let rec equal_aux first second =
      match (first, second) with
      | (End, End) -> true
      | (_, End) -> false
      | (End, _) -> false
      | (More (k1, d1, r1, e1), More (k2, d2, r2, e2)) ->
        compare_key k1 k2 = 0 && f d1 d2 && equal_aux (cons_enum r1 e1) (cons_enum r2 e1)
    in equal_aux (cons_enum first End) (cons_enum second End)

end

(* type for generic map  *)
type ('k, 'v, 'comparator) t = {tree:('k, 'v) Tree.t;
                                comparator:('k, 'comparator) Comparable.t
                               }
type ('a, 'comparator, 'z) create = ('a, 'comparator, 'z) create_options_with_comparator

(* define creator functions with comparators. *)
let empty ~comparator = {tree = Tree.empty; comparator}

let singleton ~comparator k v = {tree = Tree.singleton k v; comparator}

let of_alist ~comparator {tree;_} = Tree.of_alist tree

(* define functorized {!Creator} and {!Accessor} for Functor *)
module Creator (Key:Comparable.S1) : sig
  type ('a, 'b, 'c) create = ('a, 'b, 'c) create_options_without_comparator
  type ('k, 'v, 'c) t_ = ('k Key.t, 'v, Key.comparator) t

  include Creator with type ('k, 'v, 'c) t := ('k, 'v, 'c) t_
                  and type ('a, 'b, 'c) create := ('a, 'b, 'c) create
                  and type 'k key := 'k Key.t
end = struct
  type ('a, 'b, 'c) create = ('a, 'b, 'c) create_options_without_comparator
  type ('k, 'v, 'c) t_ = ('k Key.t, 'v, Key.comparator) t
  let comparator = Key.comparator

  let empty = {tree = Tree.empty;comparator}
  let singleton k v = singleton ~comparator k v
  let of_alist map = of_alist ~comparator map
end

module Accessor = struct
  let is_empty t = Tree.is_empty t.tree
  let add t ~key ~data = {t with tree = Tree.add t.tree ~key ~data ~compare_key:t.comparator;}
  let remove t key = {t with tree = Tree.remove t.tree key t.comparator;}
  let minimum t = try Some (Tree.min_binding t.tree) with Invalid_argument _ -> None
  let minimum_exn t = Tree.min_binding t.tree
  let maximum t = try Some (Tree.max_binding t.tree) with Invalid_argument _ -> None
  let maximum_exn t = Tree.max_binding t.tree
  let find t key = Tree.search t.tree key t.comparator
  let find_exn t key =
    match Tree.search t.tree key t.comparator with
    | None -> raise Not_found
    | Some x -> x
  let mem t key = Tree.exists t.tree key t.comparator
  let iter t f = Tree.iter t.tree f t.comparator
  let iteri t f = Tree.iteri t.tree f t.comparator
  let map t f = {t with tree = Tree.map t.tree f t.comparator}
  let mapi t f = {t with tree = Tree.mapi t.tree f t.comparator}
  let fold t ~f ~init = Tree.fold t.tree ~f ~init
  let compare f t1 t2 = Tree.compare f t1.tree t2.tree t1.comparator
  let equal f t1 t2 = Tree.equal f t1.tree t2.tree t1.comparator
  let keys t = Tree.keys t.tree
  let data t = Tree.data t.tree
end

module Functor_accessor(Key:Comparable.S) = struct
  let is_empty t = Tree.is_empty t.tree
  let add t ~key ~data = {t with tree = Tree.add t.tree ~key ~data ~compare_key:t.comparator;}
  let remove t key = {t with tree = Tree.remove t.tree key t.comparator;}
  let minimum t = try Some (Tree.min_binding t.tree) with Invalid_argument _ -> None
  let minimum_exn t = Tree.min_binding t.tree
  let maximum t = try Some (Tree.max_binding t.tree) with Invalid_argument _ -> None
  let maximum_exn t = Tree.max_binding t.tree
  let find t key = Tree.search t.tree key t.comparator
  let find_exn t key =
    match Tree.search t.tree key t.comparator with
    | None -> raise Not_found
    | Some x -> x
  let mem t key = Tree.exists t.tree key t.comparator
  let iter t f = Tree.iter t.tree f t.comparator
  let iteri t f = Tree.iteri t.tree f t.comparator
  let map t f = {t with tree = Tree.map t.tree f t.comparator}
  let mapi t f = {t with tree = Tree.mapi t.tree f t.comparator}
  let fold t ~f ~init = Tree.fold t.tree ~f ~init
  let compare f t1 t2 = Tree.compare f t1.tree t2.tree t1.comparator
  let equal f t1 t2 = Tree.equal f t1.tree t2.tree t1.comparator
  let keys t = []
  let data t = Tree.data t.tree
end

type 'k key = 'k
include Accessor

module type Key = Key
(* Used by to make key-specified {!Map} module by {!Make} *)
module type S = S
  with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

module Make_use_comparator(Key:Comparable.S) = struct
  module Key = Key

  include Creator (Comparable.S_to_S1(Key))
  type key = Key.t
  type ('a, 'b, 'c) map = (Key.t, 'b, Key.comparator) t
  type 'v t = (Key.t, 'v, Key.comparator) map
  type 'k key_ = key

  include Accessor
end

(* Make key-specified module with functor implemented
   compare between a key and define the type of key
*)
module Make(Key:Comparable.Type) = Make_use_comparator(Comparable.Make(Key))

module Poly = struct
  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type ('a, 'b) t = ('a, 'b, Comparable.Poly.comparator) map
  type 'a key = 'a

  (* creator functions *)
  include Creator (Comparable.Poly)
  (* accessors *)
  include Accessor

end
