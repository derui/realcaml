type edge_type = Convex | Concave | Flat

type t = {edge_type:edge_type;
          vertex_ids:int * int;
          face_ids: int list;
         }

let edge_type {edge_type;_} = edge_type
let vertex_ids {vertex_ids;_} = vertex_ids
let face_ids {face_ids;_} = face_ids

let make ~etype ~vertex_ids ~face_ids =
  {edge_type = etype; vertex_ids; face_ids;}
