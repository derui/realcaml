(**
   Providing some operations to manage relations between edges and relations.
   The relations is based on id of the edge contained a facet, user can find facets contains the edge.

   @version 0.1
   @author derui
*)

type t
(* The type of Edge_facet_map. *)

val make: edges:Realcaml_mesh_edge.t array -> facets:Realcaml_mesh_facet.t array -> unit -> t
(* [make ~edges ~facets ()] get the new Edge_facet_map instance. *)

val find: edge:Realcaml_mesh_types.edge_id -> t -> Realcaml_mesh_facet.t list
(* [find ~edge t] find facets contains specified edge. *)
