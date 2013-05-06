(** base operations and collection type for module of Collection. *)
module type Type =
sig
  type t
  type elt
  (** left side parenthesis when convert elt to string *)
  val lparen : string
  (** right side parenthesis when convert elt to string *)
  val rparen : string
  (** separate each elt when convert elt to string *)
  val separator : string
  (** return empty collection *)
  val empty : t
  (** add element for elt to t *)
  val add : elt -> t -> t
  (** apply each element of t with first element is 'a *)
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  include Stringable.Type with type t := elt
end

module type S =
sig
  type elt
  type t
  (** folding with index of element in t *)
  val foldi : (int -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** execute affects with index of element of t *)
  val iteri : (int -> elt -> unit) -> t -> unit
  (** append element to t if arg is Some. *)
  val add_option : elt option -> t -> t
  (** convert t from list. *)
  val of_list : elt list -> t
  (** convert list from t *)
  val to_list : t -> elt list
  (** add all element of list to given t. *)
  val add_list : elt list -> t -> t
  (** translate given lparen, rparen, separator and t into string. *)
  val to_string_sep : string -> string -> string -> t -> string

  (** Stringable Signatures *)
  include Stringable.Type with type t:= t
  include Stringable.S with type t := t
end

module Make (T:Type) : S with type elt := T.elt and type t := T.t
