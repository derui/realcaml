open Core.Std
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module Types = Realcaml_util_types

let empty () = A.Vec.zero S.three

let unit = function
  | `X -> let v = empty () in
          A.Vec.set ~index:0 ~v:1.0 v;
          v
  | `Y -> let v = empty () in
          A.Vec.set ~index:1 ~v:1.0 v;
          v
  | `Z -> let v = empty () in
          A.Vec.set ~index:2 ~v:1.0 v;
          v

let four_empty () =
  let z = A.Vec.zero S.four in
  A.Vec.set ~index:3 ~v:1.0 z;
  z

let to_three v = 
  let z = A.Vec.zero S.three in
  A.Vec.set ~index:0 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:1 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:2 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  z

let to_four v = 
  let z = four_empty () in
  A.Vec.set ~index:0 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:1 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:2 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  z

let pos_to_vec ~x ~y ~z () =
  let v = empty () in
  A.Vec.set ~index:0 ~v:x v;
  A.Vec.set ~index:1 ~v:y v;
  A.Vec.set ~index:2 ~v:z v;
  v

let extract_three v = (A.Vec.unsafe_get v 0, A.Vec.unsafe_get v 1, A.Vec.unsafe_get v 2)

let with_four v ~f = to_four v |> f |> to_three
