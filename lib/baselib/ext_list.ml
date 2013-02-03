open Num

(* return list within the compass of given min to given max. *)
let range (s, l) =
  let rec range_ cur lst =
    if cur >/ l then List.rev lst
    else range_ (succ_num cur) (cur :: lst)
  in range_ s []

let range_int (s, l) =
  List.map int_of_num (range (num_of_int s, num_of_int l))

let range_float ~step ~fromto:(s, e) =
  let rec range_ cur lst =
    if cur > e then List.rev lst
    else range_ (cur +. step) (cur :: lst)
  in range_ s []

let init n f =
  let f = fun l n -> l @ [(f n)] in
  List.fold_left f [] (range_int (0, n - 1))

let (--) mi ma = range_int (mi, ma)
let (--/) mi ma = range (mi, ma)
