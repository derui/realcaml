open Core.Std
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

module Vec = struct
  let empty =
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
    let z = empty in
    A.Vec.set ~index:0 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
    A.Vec.set ~index:1 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
    A.Vec.set ~index:2 ~v:(A.Vec.get ~index:0 v |> Option.value ~default:0.0) z;
    z
end

let to_world_transform pos orientation =
  let open A.Mat.Open in
  let module Q = Typedvec.Std.Ext.Qua in
  let module M = A.Mat in
  let module V = A.Vec in
  let orient = Q.to_mat orientation
  and trans = M.identity S.four in
  M.set ~row:0 ~col:3 ~v:(V.get pos 0 |> Option.value ~default:0.0) trans;
  M.set ~row:1 ~col:3 ~v:(V.get pos 1 |> Option.value ~default:0.0) trans;
  M.set ~row:2 ~col:3 ~v:(V.get pos 2 |> Option.value ~default:0.0) trans;
  trans *: orient
