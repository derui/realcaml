type t = {
  distance:float;
  pointA: Vecmath.Vector.t; pointB:Vecmath.Vector.t;
  normal:Vecmath.Vector.t;
  constraints: Constraint.t array;
}

let make ~dist ~pointA ~pointB ~normal ~constraints =
  {distance = dist; pointA; pointB; normal; constraints}

let distance {distance;_} = distance

let pointA {pointA;_} = pointA

let pointB {pointB;_} = pointB

let normal {normal;_} = normal

let constraints {constraints;_} = constraints
