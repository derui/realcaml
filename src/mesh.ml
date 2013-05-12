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
    normal:Vecmath.Vector.t;
  }

end

open Baselib.Std.Prelude

module V = Vecmath.Vector
module MT = Vecmath.Matrix4

module M = Baselib.Std.Map.Make(struct
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
  vertices: Vecmath.Vector.t array;
  facets:Facet.t array;
}

(* Temporary edge information to make mesh_edges *)
type edge_buffer = {
  vertex_indices:int list;
  mutable faces:int list;
}

(* TRANSLATE: 渡されたエッジが成す平面が、縮退面であるかどうかを判別する *)
let is_degenerate vertices ((aa, ab), (ba, bb), (ca, cb)) =
  let edge_a = V.sub vertices.(ab) vertices.(aa)
  and edge_b = V.sub vertices.(bb) vertices.(ba)
  and edge_c = V.sub vertices.(cb) vertices.(ca) in
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
        Printf.printf "%d\n" face_id;
        e.faces <- face_id :: e.faces;
        buf
    ) buf [ea; eb; ec] in
  (* TODO faceのリストから、エッジのmapを作成する *)
  Array.iteri (fun i e ->
    edge_buf := register_face_edges !edge_buf i (edges_of_face e)) faces;
  !edge_buf

let convert ~vertices ~faces =
  (* TODO 縮退面を事前に削除する。 *)
  let faces = Array.of_list |< List.filter (fun e ->
    let (ea, eb, ec) = edges_of_face e in
    is_degenerate vertices (ea, eb, ec)
  ) (Array.to_list faces) in

  let edge_buf = convert_edges vertices faces in
  (* TODO 作成したmapを、Edgeのリストとして作成する。この時点では、すべてのedgeを凸メッシュとして
     作成する。*)
  let edge_buf = Array.of_list |< M.fold edge_buf ~f:(fun ~key ~data l ->
    {Edge.edge_type = Edge.Convex; vertex_ids = (List.nth data.vertex_indices 0, List.nth data.vertex_indices 1);
     face_ids = data.faces
    } :: l
  ) ~init:[] in
  (* TODO faceの配列を、Facetの配列として作成する。Edgeの配列はすでに用意されているので、
     配列の内部から、一致するindexを取得して利用する。
  *)
  let faces = Array.map (fun (va, vb, vc) ->
    let (ea, eb, ec) = edges_of_face (va, vb, vc) in
    let edge_detect (origin_a, origin_b) edge =
      let (ea, eb) = edge.Edge.vertex_ids in
      if origin_a = ea && origin_b = eb then true
      else if origin_a = eb && origin_b = ea then true
      else false in
    let ea = array_find edge_buf |< edge_detect ea
    and eb = array_find edge_buf |< edge_detect eb
    and ec = array_find edge_buf |< edge_detect ec in
    match (ea, eb, ec) with
    | (None,_,_) | (_,None,_) | (_,_,None) -> failwith "not found some edge of face"
    | (Some ea, Some eb, Some ec) ->
      let normal = V.normalize |< V.cross (V.sub vertices.(vb) vertices.(va))
        (V.sub vertices.(vc) vertices.(va)) in
      {Facet.vertex_ids = (va, vb, vc); edge_ids = (ea, eb, ec);normal}
  ) faces in
  {vertices = vertices; edges = edge_buf; facets = faces;}


let transform_vertices mesh mat =
  let new_vertices = Array.map (fun vec -> MT.mult_vec ~mat ~vec) mesh.vertices in
  {mesh with vertices = new_vertices}
