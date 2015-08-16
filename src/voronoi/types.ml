
type vec = Realcaml_util.Types.vec
type mat = Realcaml_util.Types.mat

type triangle = vec * vec * vec
(* The type of triangle. *)

(* This type have closest point of the triangle and the place of the triangle. *)
(* TRANSLATE: Pointは最近接点が、いずれかの頂点。
   Edgeは、最近接点が、いずれかのエッジ上に存在する。
   Shapeは、平面上に最近接点が存在する。
   それぞれ最近接点が含まれる
*)
type recent_type = Point of vec
                   | Edge of vec
                   | Shape of vec
