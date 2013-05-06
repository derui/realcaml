module Core_ = struct
  type ('a, 'b) t =
  | Left of 'b
  | Right of 'a

  let return x = Right x
  let bind (m : ('a, 'b) t) f =
    match m with
      | Left x -> Left x
      | Right x -> f x
end

type ('a, 'b) t = ('a, 'b) Core_.t =
  | Left of 'b
  | Right of 'a

let either_left e f =
  match e with
  | Left s -> Some (f s)
  | Right _ -> None

let either_right e f =
  match e with
  | Left _ -> None
  | Right s -> Some (f s)

let is_left = function
  | Left _ -> true
  | Right _ -> false

let is_right e = not (is_left e)


include Monad.Make2(Core_)
