module Voronoi = Realcamel.Voronoi
open OUnit

(* Triangle on the x-z plane. *)
let vertices () =
  let open Candyvec.Vector in
  [| {x = 0.0;y = 1.0;z = 0.0;};
     {x = 0.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 1.0;z = 0.0;};
  |]

let faces () = [|(0, 1, 2);
              (1, 2, 3);
            |]

let test_mesh_conversion _ =
  let mesh = Mesh.convert ~vertices:(vertices ()) ~faces:(faces ()) in
  let vertices = mesh.Mesh.vertices 
  and edges = mesh.Mesh.edges
  and facets = mesh.Mesh.facets in
  assert_equal 4 (Array.length vertices);
  assert_equal 5 (Array.length edges);
  assert_equal 2 (Array.length facets);
  let facet_vertices = facets.(0).Mesh.Facet.vertex_ids in
  assert_equal (0, 1, 2) (facet_vertices)

let suite = "mesh convert specs" >::: [
  "normal convert" >:: test_mesh_conversion
]
