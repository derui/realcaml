open Core.Std

module Edge = Realcaml_mesh_edge
module Facet = Realcaml_mesh_facet
module Types = Realcaml_mesh_types

module M = Map.Make(struct
  type t = Types.edge_id
  let t_of_sexp = Int.t_of_sexp
  let sexp_of_t = Int.sexp_of_t
  let compare = Pervasives.compare
end)

type t = Facet.t list M.t

let has_edge_in_facet edge facet = 
  let module E = Edge in
  let module F = Facet in
  let (ea, eb, ec) = facet.F.edge_ids in
  ea = edge.E.edge_id || eb = edge.E.edge_id || ec = edge.E.edge_id

let make ~edges ~facets () =
  let m = ref M.empty in
  let module E = Edge in

  Array.iter edges ~f:(fun edge ->
    let facets = Array.filter facets ~f:(has_edge_in_facet edge) in
    m := M.add !m ~key:edge.E.edge_id ~data:(Array.to_list facets)
  );
  !m

let find ~edge t =
  let module E = Edge in M.find t edge |> Option.value ~default:[]
