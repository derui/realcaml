type t = {vertex_ids:int * int * int;
          edge_ids: int * int * int;
          normal:Vecmath.Vector.t;
         }

let vertex_ids {vertex_ids;_} = vertex_ids
let edge_ids {edge_ids;_} = edge_ids
let normal {normal;_} = normal

let make ~vertex_ids ~edge_ids ~normal =
  {vertex_ids; edge_ids; normal}
