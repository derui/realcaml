open Prelude

let compare ?(epsilon=0.00001) f1 f2 =
  let diff = f1 -. f2 |> abs_float in
  if diff < epsilon then 0
  else if f1 < f2 then -1
  else 1
