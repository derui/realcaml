module type Type =
sig
  type 'a t
  val empty : 'a t
  (* add element for elt to t *)
  val add : 'a -> 'a t -> 'a t
  (* apply each element of t with first element is 'a *)
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type S =
sig
  type 'a t
  (* folding with index of element in t *)
  val foldi : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (* execute affects with index of element of t *)
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (* append element to t if arg is Some. *)
  val add_option : 'a option -> 'a t -> 'a t
  (* convert t from list. *)
  val of_list : 'a list -> 'a t
  (* convert list from t *)
  val to_list : 'a t -> 'a list
  (* add all element of list to given t. *)
  val add_list : 'a list -> 'a t -> 'a t
end

module Make (T:Type) : S with type 'a t := 'a T.t =
struct
  open T
  let foldi f =
    let i = ref (-1) in
    fold (fun e t -> i := !i + 1; f !i e t)

  let iteri f t =
    foldi (fun i e () -> f i e; ()) t ()

  let add_option = function
    | None -> Prelude.id
    | Some x -> add x

  let of_list l =
    List.fold_right add l empty

  let to_list t =
    List.rev (fold (fun x l -> x::l) t [])

  let add_list l t =
    List.fold_left (Prelude.flip add) t l

end
