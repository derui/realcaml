module Mesh = Realcamel.Mesh
open OUnit

let vertices =
  let open Candyvec.Vector in
  [| {x = 0.0;y = 1.0;z = 0.0;};
     {x = 0.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 0.0;z = 0.0;};
     {x = 1.0;y = 1.0;z = 0.0;};
  |]

let faces = [|(0, 1, 2);
              (1, 2, 3);
            |]

let test_mesh_conversion _ =
  let mesh = Mesh.convert ~vertices ~faces in
  let vertices = mesh.Mesh.vertices
  and edges = mesh.Mesh.edges
  and facets = mesh.Mesh.facets in
  assert_equal ~printer:(Printf.sprintf "%d") 4 (Array.length vertices);
  assert_equal ~printer:(Printf.sprintf "%d") 5 (Array.length edges);
  assert_equal ~printer:(Printf.sprintf "%d") 2 (Array.length facets);
  let facet_vertices = facets.(0).Mesh.Facet.vertex_ids in
  assert_equal (0, 1, 2) (facet_vertices)
;;

let base_vertices =
  let open Candyvec.Vector in
  [| {x = 0.0; y = 1.0; z = 0.0;};
     {x = -.1.0; y = 0.0; z = 1.0;};
     {x = 1.0; y = 0.0; z = 1.0;};
     {x = 1.0; y = 0.0; z = -.1.0;};
     {x = -.1.0; y = 0.0; z = -.1.0;};
     {x = 0.0; y = -1.0; z = 0.0};
  |]
;;

let base_faces =
  [| (0, 1, 2);
     (0, 2, 3);
     (0, 3, 4);
     (0, 4, 1);
     (5, 2, 1);
     (5, 3, 2);
     (5, 4, 3);
     (5, 1, 4);
  |]
;;

let test_octahedron_mesh_conversion _ =
  let mesh = Mesh.convert ~vertices:base_vertices ~faces:base_faces in
  let vertices = mesh.Mesh.vertices
  and edges = mesh.Mesh.edges
  and facets = mesh.Mesh.facets in
  assert_equal ~printer:(Printf.sprintf "%d") 6 (Array.length vertices);
  assert_equal ~printer:(Printf.sprintf "%d") 12 (Array.length edges);
  assert_equal ~printer:(Printf.sprintf "%d") 8 (Array.length facets);
  let facet_vertices = facets.(0).Mesh.Facet.vertex_ids in
  assert_equal (0, 1, 2) (facet_vertices)
;;

let suite = "mesh convert specs" >::: [
  "normal convert" >:: test_mesh_conversion;
  "octahedron convert" >:: test_octahedron_mesh_conversion;
]
