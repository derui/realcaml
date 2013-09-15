open OUnit

let suite = "realcamel tests" >:::  [
  Mesh_test.suite;
  Voronoi_test.suite;
  SeparateAxis_test.suite;
]

let _ =
  run_test_tt_main suite






