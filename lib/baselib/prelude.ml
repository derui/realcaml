open Num

(* identity function *)
let id t = t
let const x _ = x
let flip f x y = f y x

let (|>) f g = g f
let (|<) f g = f g
let (@<) f g = (fun x -> f (g x))
let (@>) f g = (fun x -> g (f x))

let cmp_float ~epsilon f1 f2 =
  let sub = (f1 -. f2) in
  let sub_abs = abs_float sub in
  if sub_abs <= epsilon then 0
  else if sub < 0.0 then -1
  else 1

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b
