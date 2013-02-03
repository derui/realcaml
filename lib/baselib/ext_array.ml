
let remove origin ~index ~init =
  let new_array = Array.make (Array.length origin) init in
  let first_length = index in
  let second_pos = index + 1 in
  let second_length = (Array.length origin) - (first_length + 1) in
  Array.blit origin 0 new_array 0 first_length;
  Array.blit origin second_pos new_array ((Array.length new_array) - 1) second_length;
  new_array

let add origin element = Array.append origin [|element|]
