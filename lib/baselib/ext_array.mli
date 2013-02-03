(** Provide some functions for ocaml array.
    Containing functions provided are not included Array module of the stdlib.

    @author derui
    @version 0.2
*)

(** Remove an element at an given index and return new array removed element.
    This function uses Array.blit twice for coping array.

    @param array an array to remove an element from
    @param index the index of an element to remove from array
    @param init an element to initialize new array.
    @return new array that is copied original array without a removed element.
*)
val remove : 'a array -> index:int -> init:'a -> 'a array

(** Add an element to tail of the array and return new one.
    Therefore this function is very slow because it copies given array
    to new array that size is more larger than original one.

    @param array the array to add an element to.
    @param element the element to add to the array
    @return new array added a given element.
*)
val add : 'a array -> 'a -> 'a array
