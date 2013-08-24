module Edge = struct
  type edge_type = Convex | Concave | Flat

  (** A type of Edge  *)
  type t = {
    edge_type:edge_type;
    vertex_ids:int * int;
    face_ids: int list;
  }
end

module Facet = struct
  type t = {
    vertex_ids:int * int * int;
    edge_ids: int * int * int;
    normal:Candyvec.Vector.t;
  }

end

open Sugarpot.Std.Prelude

module V = Candyvec.Vector
module MT = Candyvec.Matrix4

module M = Sugarpot.Std.Map.Make(struct
  type t = int * int
  let compare (aa, ab) (ba, bb) =
    if Pervasives.compare aa ba = 0 then
      Pervasives.compare ab bb
    else Pervasives.compare aa ba
end)
let max_vertices = 34
let max_edges = 96
let max_facets = 64

type t = {
  edges: Edge.t array;
  vertices: Candyvec.Vector.t array;
  facets:Facet.t array;
}

(* Temporary edge information to make mesh_edges *)
type edge_buffer = {
  vertex_indices:int list;
  mutable faces:int list;
}

(* TRANSLATE: 渡されたエッジが成す平面が、縮退面であるかどうかを判別する *)
let is_degenerate vertices ((aa, ab), (ba, bb), (ca, cb)) =
  let edge_a = V.normalize (V.sub vertices.(ab) vertices.(aa))
  and edge_b = V.normalize (V.sub vertices.(bb) vertices.(ba))
  and edge_c = V.normalize (V.sub vertices.(cb) vertices.(ca)) in
  if V.dot edge_a (V.invert edge_c) = 1.0 then true
  else if V.dot edge_b (V.invert edge_a) = 1.0 then true
  else if V.dot edge_c (V.invert edge_b) = 1.0 then true
  else false

let array_find ary f =
  let len = Array.length ary in
  let rec loop ary index f =
    if index = len then None
    else if f |< Array.get ary index then
      Some index
    else
      loop ary (succ index) f in
  loop ary 0 f


let edges_of_face (ai, bi, ci) = ((ai, bi), (bi, ci), (ci, ai))
;;

let convert_edges vertices faces =
  let edge_buf = ref M.empty in

  let register_face_edges buf face_id (ea, eb, ec) =
    List.fold_left (fun buf edge ->
      let swaped_edge = let (a, b) = edge in (b, a) in
      match (M.find buf edge, M.find buf swaped_edge) with
      | (None, None) ->
        let (indexa, indexb) = edge in
        M.add buf ~key:edge ~data:{vertex_indices = [indexa; indexb];
                                   faces = [face_id];}
      | (Some e, _) | (_, Some e) ->
        e.faces <- face_id :: e.faces;
        buf
    ) buf [ea; eb; ec] in
  (* TODO faceのリストから、エッジのmapを作成する *)
  Array.iteri (fun i e ->
    edge_buf := register_face_edges !edge_buf i (edges_of_face e)) faces;
  !edge_buf
;;

let equal_edge (e1, e2) (e21, e22) =
  (e1 = e21 && e2 = e22) || (e2 = e21 && e1 = e22)
;; 

(* Get faces what include the edge is given *)
let get_faces_contain_edge faces edge =
  List.filter (fun face ->
    let ea, eb, ec = edges_of_face face in
    List.for_all (equal_edge edge) [ea;eb;ec]
  ) faces 
;;

let convert ~vertices ~faces =
  (* TODO 縮退面を事前に削除する。 *)
  let faces = List.filter (fun e ->
    let (ea, eb, ec) = edges_of_face e in
    not (is_degenerate vertices (ea, eb, ec))
  ) (Array.to_list faces) in
  let open Sugarpot.Std.List in
  let face_ids = range_int (0, List.length faces - 1) in
  let faces = zip (fun i d -> (i, d)) face_ids faces in

  let edge_buf = List.fold_left (fun buf (i, face) ->
    let ea,eb,ec = edges_of_face face in
    let register_face_edge buf edge = 
      let swaped_edge = let (a, b) = edge in (b, a) in
      match (M.find buf edge, M.find buf swaped_edge) with
      | (None, None) ->
        M.add buf ~key:edge ~data:{
          Edge.edge_type = Edge.Convex;
          vertex_ids = edge;
          face_ids = [i];}
      | (Some e, _) | (_, Some e) ->
        M.add buf ~key:edge ~data:{e with Edge.face_ids = i :: e.Edge.face_ids} in
    List.fold_left register_face_edge buf [ea; eb; ec]
  ) M.empty faces in
  let edge_ary = Array.of_list (M.data edge_buf) in

  (* TODO faceの配列を、Faceの配列として作成する。Edgeの配列はすでに用意されているので、
     配列の内部から、一致するindexを取得して利用する。
  *)
  let faces = List.map (fun (index, face) ->
    let (ea, eb, ec) = edges_of_face face in

    let edge_detect base_edge edge =
      let e1, e2 = base_edge in
      let swapped = (e2, e1) in
      let key = edge.Edge.vertex_ids in
      if (equal_edge base_edge key || equal_edge swapped key) then
        match M.find edge_buf key with
        | None -> false
        | Some e ->
          let ids = e.Edge.face_ids in
          List.exists ((=) index) ids
      else false in
    let ea = array_find edge_ary |< edge_detect ea
    and eb = array_find edge_ary |< edge_detect eb
    and ec = array_find edge_ary |< edge_detect ec in
    match (ea, eb, ec) with
    | (None,_,_) | (_,None,_) | (_,_,None) -> failwith "not found some edge of face"
    | (Some ea, Some eb, Some ec) ->
      let va, vb, vc = face in
      let normal = V.normalize |< V.cross (V.sub vertices.(vb) vertices.(va))
        (V.sub vertices.(vc) vertices.(va)) in
      {Facet.vertex_ids = (va, vb, vc); edge_ids = (ea, eb, ec);normal}
  ) faces in
  {vertices = vertices; edges = edge_ary; facets = Array.of_list faces;}
;;

let transform_vertices mesh mat =
  let new_vertices = Array.map (fun vec -> MT.mult_vec ~mat ~vec) mesh.vertices in
  {mesh with vertices = new_vertices}
