(**
   Provide some functions for operation with float.

   @author derui
   @version 0.1
*)

(** Compare two float. Argument epsilon is maximum difference with two float
    allowed to equal to two float.
    The default value of epsilon is 0.000001, then you specify epsilon if you need more or less accuracy.

    @param f1 a float to compare with another float
    @param f2 a float to compare with another float
    @param epsilon maximum difference between two float to be allowed equivalent
    @return -1 if f1 is less than f2, 1 if f1 is greater than f2, 0 if f1 equal to f2
*)
val compare : ?epsilon:float -> float -> float -> int
