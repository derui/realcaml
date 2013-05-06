let to_string_sep rparen lparen f = function
  | None -> rparen ^ lparen
  | Some x -> rparen ^ (f x) ^ lparen

let to_string f = to_string_sep "{" "}" f

let print fmt f x = Format.fprintf fmt "%s" (to_string f x)

let is_none = function
  | None -> true
  | _ -> false

let is_some = function
  | Some _ -> true
  | _ -> false

let compare_with ?(comparator=compare) fst snd =
  match (fst, snd) with
  | None, None -> 0
  | Some _, None -> 1
  | None , Some _ -> -1
  | Some x, Some y -> comparator x y

let map f list =
  let rec map_ f list build =
    match list with
    | [] -> build
    | Some (x) :: rest -> map_ f rest (f x :: build)
    | None :: rest -> map_ f rest build in
  map_ f list []

module Core =
struct
  type 'a t = 'a option
  let empty = None
  let identity = None
  let return x = Some x
  let some = return
  let add x _ = Some x

  let bind x f =
    match x with
    | None -> None
    | Some x -> f x

  let fold f x e =
    match x with
    | None -> e
    | Some x -> f x e

  let safe_get e = function
    |  None -> e
    | Some x -> x
end

include Core
include Monad.Make(Core)
include TypedCollection.Make(Core)
