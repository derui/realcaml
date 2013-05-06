
module type Type =
sig
  type 'a t
  val to_string : 'a t -> string
end

module type S =
sig
  type 'a t
  val print : Format.formatter -> 'a t -> unit
  val to_chararray : 'a t -> char array
  val to_charlist : 'a t -> char list
end

module Make (T : Type) : S with type 'a t := 'a T.t =
struct
  open T

  let print fmt t = Format.fprintf fmt "%s" (to_string t)

  let to_chararray t =
    let s = to_string t in
    Array.init (String.length s) (String.get s)

  let to_charlist t =
    Array.to_list (to_chararray t)
end
