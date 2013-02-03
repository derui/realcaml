type t = {
  contact_num:int;
  friction:float;
  contact_points:ContactPoint.t array;
}

let make ~num ~friction ~points =
  {contact_num = num; friction; contact_points = points}

let empty = {contact_num = 0; friction = 0.0; contact_points = [||]}

let contact_num {contact_num;_} = contact_num

let friction {friction;_} = friction

let contact_points {contact_points;_} = contact_points
