
module type Type =
sig
  type t
  type elt
  val lparen : string
  val rparen : string
  val separator : string
  val empty : t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  include Stringable.Type with type t := elt
end

module type S =
sig
  type elt
  type t
  val foldi : (int -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iteri : (int -> elt -> unit) -> t -> unit
  val add_option : elt option -> t -> t
  val of_list : elt list -> t
  val to_list : t -> elt list
  val add_list : elt list -> t -> t
  val to_string_sep : string -> string -> string -> t -> string

  include Stringable.Type with type t := t
  include Stringable.S with type t := t
end

module Make (T:Type) : S with type elt := T.elt and type t := T.t =
struct
  type t = T.t
  type elt = T.elt
  let foldi f =
    let i = ref (-1) in
    T.fold (fun e t -> i := !i + 1; f !i e t)

  let iteri f t =
    foldi (fun i e () -> f i e; ()) t ()

  let add_option = function
      None -> Prelude.id
    | Some x -> T.add x

  let of_list l =
    List.fold_right T.add l T.empty

  let to_list t =
    List.rev (T.fold (fun x l -> x::l) t [])

  let add_list l t =
    List.fold_left (Prelude.flip T.add) t l

  let to_string_sep lparen rparen sep t =
    let adds i e s = s ^ (if i <> 0 then sep else "") ^ (T.to_string e)
    in
    let s = foldi adds t "" in
    lparen ^ s ^ rparen

  let to_string = to_string_sep T.lparen T.rparen T.separator

  include Stringable.Make((struct
    type t = T.t
    let to_string = to_string
  end : Stringable.Type with type t = T.t))
end

