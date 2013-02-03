open Baselib.Std.Prelude

type pair = New | Keep

type t = {
  mutable pair_type:pair;
  key:int64;
  mutable contact:Contact.t;
  indexA:int32; indexB:int32;
}

let make_by_key ~pt ~key ?contact () =
  let open Int64 in
  let indexA = to_int32 |< shift_right key 32
  and indexB = to_int32 key in
  {pair_type = pt; key; indexA;indexB;
   contact = match contact with | None -> Contact.empty | Some contact -> contact;
  }

let make_by_index ~pt ~indexA ~indexB ?contact () =
  let open Int64 in
  let a_int64 = flip shift_left 32 |< of_int32 indexA
  and b_int64 = of_int32 indexB in
  {pair_type = pt; key = logor a_int64 b_int64;
   indexA; indexB;
   contact = match contact with | None -> Contact.empty | Some contact -> contact;
  }

let empty = {
  pair_type = New; key = 0L;
  contact = Contact.empty ; indexA = 0l; indexB = 0l;}

let pair_type {pair_type;_} = pair_type

let key {key;_} = key

let indexOfA {indexA;_} = indexA

let indexOfB {indexB;_} = indexB

let contact {contact;_} = contact

let update_contact pair ct = pair.contact <- ct; pair

let change_pair pair new_type = pair.pair_type <- new_type; pair;
