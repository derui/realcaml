open Core.Std
module C = Realcaml_contact.Contact

type pair = New | Keep | Empty

type t = {
  pair_type:pair;
  key:int64;
  contact:C.t;
  indexA:int32; indexB:int32;
}

let make_by_key ~pt ~key ?contact () =
  let open Int64 in
  let indexA = shift_right key 32 |> to_int32 |> Option.value ~default:0l
  and indexB = to_int32 key |> Option.value ~default:0l in
  {pair_type = pt;
   key;
   indexA;
   indexB;
   contact = Option.value ~default:C.empty contact;
  }

let make_by_index ~pt ~indexA ~indexB ?contact () =
  let open Int64 in
  let a_int64 = of_int32 indexA |> Fn.flip shift_left 32
  and b_int64 = of_int32 indexB in
  {pair_type = pt;
   key = bit_or a_int64 b_int64;
   indexA; indexB;
   contact = Option.value ~default:C.empty contact;
  }

let empty = {
  pair_type = Empty;
  key = Int64.max_value;
  contact = C.empty;
  indexA = Int32.max_value;
  indexB = Int32.max_value;
}
