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

module Make (T:Type) : S with type 'a t := 'a T.t
