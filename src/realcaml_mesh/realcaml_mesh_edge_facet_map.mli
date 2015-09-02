(**
   Providing some operations to manage relations between edges and relations.
   The relations is based on id of the edge contained a facet, user can find facets contains the edge.

   @version 0.1
   @author derui
*)

module Edge = Realcaml_mesh_edge
module Facet = Realcaml_mesh_facet
module Types = Realcaml_mesh_types

type t
(* The type of Edge_facet_map. *)

val make: edges:Edge.t array -> facets:Facet.t array -> unit -> t
(* [make ~edges ~facets ()] get the new Edge_facet_map instance. *)

val find: edge:Types.edge_id -> t -> Facet.t list
(* [find ~edge t] find facets contains specified edge. *)
