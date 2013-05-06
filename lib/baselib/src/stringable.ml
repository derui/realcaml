
module type Type =
sig
  type t
  val to_string : t -> string
end

module type S =
sig
  type t
  val print : Format.formatter -> t -> unit
  val to_chararray : t -> char array
  val to_charlist : t -> char list
end

module Make (T : Type) : S with type t := T.t =
struct
  open T

  let print fmt t = Format.fprintf fmt "%s" (to_string t)

  let to_chararray t =
    let s = to_string t in
    Array.init (String.length s) (String.get s)

  let to_charlist t =
    Array.to_list (to_chararray t)
end
