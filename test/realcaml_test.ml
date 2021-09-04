let () = Alcotest.run "Realcaml" [ ("Mesh", Mesh_test.tests); ("Rigid body", Rigid_body_test.tests) ]
