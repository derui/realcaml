type t = {
  contact_num:int;
  friction:float;
  contact_points:ContactPoint.t list;
}

let empty = {contact_num = 0; friction = 0.0; contact_points = []}
