open Core.Std
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size
module Types = Realcaml_util_types

let empty = A.Vec.zero S.three

let four_empty =
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
  let z = four_empty in
  A.Vec.set ~index:0 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:1 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  A.Vec.set ~index:2 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
  z
