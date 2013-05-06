
module type Type =
  sig
    include Comparable.Type
    include Stringable.Type with type t := t
  end

module type S =
  sig
    include Comparable.S
    include Stringable.S with type t := t
  end

module Make (T:Type) : S with type t := T.t =
struct
  include Comparable.Make(T)
  include Stringable.Make(T)
end
