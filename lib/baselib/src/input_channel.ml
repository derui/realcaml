let read ~chan ~len =
  let buffer = String.create len in
  try
    really_input chan buffer 0 len;
    Some buffer
  with End_of_file -> None


let seek chan ~from ~pos =
  match from with
  | `Current -> seek_in chan ((pos_in chan) + pos)
  | `Head -> seek_in chan pos
  | `Tail -> seek_in chan ((in_channel_length chan) - pos)

let ahead chan len =
  let open Option.Open in
  read ~chan ~len >>= (fun buf ->
    seek chan ~from:`Current ~pos:(-len);
    return buf
  )
