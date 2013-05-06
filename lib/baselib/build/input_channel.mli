(** Implements functions for input channel as extended Pervasive's.
    Provided functions are able to split some categories, such as
    binary, text, and useful input funcitons.

    @author derui
    @version 0.2
*)

(** Read exactly len charactors from current position of the input channel.
    If in_channel reached end of file, return None.

    @param chan input channel to read charactors
    @param len reading length from input channel
    @return readed charactors
*)
val read : chan:in_channel -> len:int -> string option

(** This sets the current reading position to pos for channel chan.
    You can select some methods to detect setting the current reading position as follows:
    `Current -> Set the current reading position to add pos to the current reading position.
    `Head -> Set the current reading position to set it to directly pos.
    `Tail -> Set the current reading position to subtract pos to the tail of channel chan.

    Note: You must set negative value to pos if `Tail selected.

    @param in_channel the input channel to set the current reading position to pos for.
    @param from the method to detect start position for setting the current reading position.
    @param pos the position to set the current reading position to.
*)
val seek : in_channel -> from:[< `Current | `Head | `Tail]
  -> pos:int -> unit

(** Read exactly len charactors from the current reading position of the channel chan,
    and rewind the channel chan to head of it.

    @param chan input channel to read charactors
    @param len reading length from input channel
    @return readed charactors
*)
val ahead : in_channel -> int -> string option
