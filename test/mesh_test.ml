open Realcaml
module V = Typedvec.Algebra.Vec
module S = Typedvec.Size

let to_vert ~x ~y ~z () =
  let v = V.make S.three 0.0 in
  V.set v ~index:0 ~v:x;
  V.set v ~index:1 ~v:y;
  V.set v ~index:2 ~v:z;
  v

let vertices =
  [|
    to_vert ~x:0.0 ~y:1.0 ~z:0.0 ();
    to_vert ~x:0.0 ~y:0.0 ~z:0.0 ();
    to_vert ~x:1.0 ~y:0.0 ~z:0.0 ();
    to_vert ~x:1.0 ~y:1.0 ~z:0.0 ();
  |]

let faces = [| (0, 1, 2); (1, 2, 3) |]

let tests =
  let base_vertices =
    [|
      to_vert ~x:0.0 ~y:1.0 ~z:0.0 ();
      to_vert ~x:(-1.0) ~y:0.0 ~z:1.0 ();
      to_vert ~x:1.0 ~y:0.0 ~z:1.0 ();
      to_vert ~x:1.0 ~y:0.0 ~z:(-1.0) ();
      to_vert ~x:(-1.0) ~y:0.0 ~z:(-1.0) ();
      to_vert ~x:0.0 ~y:(-1.0) ~z:0.0 ();
    |]
  in

  let base_faces = [| (0, 1, 2); (0, 2, 3); (0, 3, 4); (0, 4, 1); (5, 2, 1); (5, 3, 2); (5, 4, 3); (5, 1, 4) |] in
  [
    ( "Mesh can convert vertices and faces to mesh",
      `Quick,
      fun () ->
        let open Mesh in
        let mesh = Mesh.convert ~vertices ~faces () in
        let vertices = mesh.Mesh.vertices and edges = mesh.Mesh.edges and facets = mesh.Mesh.facets in
        Alcotest.(check int) "vertices" 4 @@ Array.length vertices;
        Alcotest.(check int) "edges" 5 @@ Array.length edges;
        Alcotest.(check int) "facets" 2 @@ Array.length faces;
        let facet_vertices = facets.(0).Facet.vertex_ids in
        let facet_vertices_testable = Alcotest.testable Fmt.nop Stdlib.( = ) in
        Alcotest.(check facet_vertices_testable) "facet vertices" (0, 1, 2) facet_vertices );
    ( "Mesh must have mapping for edge-face relations",
      `Quick,
      fun () ->
        let open Mesh in
        let mesh = Mesh.convert ~vertices ~faces () in
        let face = mesh.Mesh.facets.(0) in
        let mapping = mesh.Mesh.edge_facet_map in
        let module M = Edge_facet_map in
        let ea, eb, ec = face.Facet.edge_ids in
        let edge_to_length edge = M.find ~edge mapping |> List.length in
        let facets = List.sort compare [ edge_to_length ea; edge_to_length eb; edge_to_length ec ] in
        Alcotest.(check @@ list int) "facets" [ 1; 1; 2 ] facets );
    ( "Mesh can convert octahedron to mesh",
      `Quick,
      fun () ->
        let open Mesh in
        let mesh = Mesh.convert ~vertices:base_vertices ~faces:base_faces () in
        let vertices = mesh.Mesh.vertices and edges = mesh.Mesh.edges and facets = mesh.Mesh.facets in
        Alcotest.(check int) "vertices" 6 @@ Array.length vertices;
        Alcotest.(check int) "edges" 12 @@ Array.length edges;
        Alcotest.(check int) "facets" 8 @@ Array.length facets;
        let facet_vertices = facets.(0).Facet.vertex_ids in
        let facet_vertices_testable = Alcotest.testable Fmt.nop Stdlib.( = ) in
        Alcotest.(check facet_vertices_testable) "facet vertices" (0, 1, 2) facet_vertices );
  ]
