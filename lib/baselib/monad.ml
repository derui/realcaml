open Monad_intf

module Make (T:Type) : S with type 'a t := 'a T.t =
struct
  include T

  module Open_ = struct
    let bind = T.bind
    let (>>=) = T.bind
    let return = T.return
  end
  include Open_
  module Open = struct
    type 'a t = 'a T.t
    include Open_
  end

  let map f m = m >>= fun m -> return (f m)

  let rec sequence = function
    | [] -> return []
    | x::xs ->
      x >>= fun x ->
      sequence xs >>= fun xs ->
      return (x::xs)
end

module Make2 (T:Type2) : S2 with type ('a, 'z) t := ('a, 'z) T.t =
struct

  include T
  module Open_ = struct
    let bind = T.bind
    let (>>=) = T.bind
    let return = T.return
  end
  include Open_
  module Open = struct
    type ('a, 'z) t = ('a, 'z) T.t
    include Open_
  end

  let map f m = m >>= fun m -> return (f m)

  let rec sequence = function
    | [] -> return []
    | x::xs ->
      x >>= fun x ->
      sequence xs >>= fun xs ->
      return (x::xs)
end
