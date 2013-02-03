module Mesh = Camelback.Mesh
module Mesh_facet = Camelback.Mesh_facet
open OUnit

let vertices () =
  let open Vecmath.Vector in
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
  let vertices = Mesh.vertices mesh
  and edges = Mesh.edges mesh
  and facets = Mesh.facets mesh in
  assert_equal 4 (Array.length vertices);
  assert_equal 5 (Array.length edges);
  assert_equal 2 (Array.length facets);
  let facet_vertices = Mesh_facet.vertex_ids facets.(0) in
  assert_equal (0, 1, 2) (facet_vertices)

let suite = "mesh convert specs" >::: [
  "normal convert" >:: test_mesh_conversion
]

let _ =
  run_test_tt_main suite
