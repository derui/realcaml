(** Utility functions for conversion binary data to OCaml data, or OCaml data to binary data.
    These functions are used with in/out channel and binary readed buffer.

    In addition, this module include conversion function for to convert C-type float (4byte).

    This module privides functions for string to pack and unpack values.

    @author derui
    @version 0.2
*)

(** endianness of binary data *)
type endian = Little_endian | Big_endian

(** Exception for invalid byte position when calculate offset *)
exception Binary_pack_invalid_byte_range of int
exception Binary_pack_invalid_short_range of int
exception Binary_pack_invalid_unsigned_short_range of int
exception Binary_pack_invalid_int_position of int
exception Binary_pack_invalid_int64_position of int
exception Binary_pack_invalid_float_position of int

(** Exception for invalid buffer position to write unpacked value. *)
exception Binary_pack_invalid_buffer_position of int


(** The module is used by argument for Binary_pack functions
    instead of charactor array that equals string.
*)
module type BinaryArray = sig
  type t
  val length: t -> int
  val get: t -> int -> char
  val set: t -> int -> char -> unit
end

(** Functions that is used to pack and unpack values.
*)
module type S = sig
  type t

  (** Pack unsigned char data of the buffer at pos to OCaml int.
      Use a element of the buffer.
      Limits packed value between 0 to 255.

      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 0 to 255.
      @raise Bnary_pack_invalid_byte_position
  *)
  val unpack_unsigned_8 : buffer:t -> pos:int -> int

  (** Unpack unsigned char data by given OCaml int, and
      write unpacked value to the buffer at the pos.
      Limits unpacked value between 0 to 255.
      If given value to pack is greater than 255, write 0 to buffer.

      @param buffer the buffer to write unpacked value
      @param pos the position at the buffer to write unpacked value
      @param value the value to unpack
      @raise Bnary_pack_invalid_buffer_position
  *)
  val pack_unsigned_8 : buffer:t -> pos:int -> int -> unit

  (** Pack signed char data of the buffer at pos to OCaml int.
      see {!pack_unsigned_8}
  *)
  val unpack_signed_8 : buffer:t -> pos:int -> int

  (** Unpack signed char data of the buffer at pos to OCaml int.
      see {!pack_unsigned_8}
  *)
  val pack_signed_8 : buffer:t -> pos:int -> int -> unit

  (** Pack signed short data of the buffer at pos to OCaml int.
      Use two element beginning of the buffer from pos.
      Limits packed value between 32767 to -32768.

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 0 to 255.
      @raise Bnary_pack_invalid_byte_position
  *)
  val unpack_signed_16 : byte_order:endian -> buffer:t -> pos:int -> int

  (** Unpack signed short data from given OCaml int.
      Write unpacked value to the buffer from the pos
      Limits packed value between 32767 to -32768.

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 0 to 255.
      @raise Bnary_pack_invalid_short_position
  *)
  val pack_signed_16 : byte_order:endian -> buffer:t -> pos:int -> int -> unit

  (** see {!pack_signed_16} *)
  val unpack_unsigned_16 : byte_order:endian -> buffer:t -> pos:int -> int
  (** see {!unpack_signed_16} *)
  val pack_unsigned_16 : byte_order:endian -> buffer:t -> pos:int -> int -> unit

  (** Pack signed integer data of the buffer at pos to int32.
      Use four element beginning of the buffer from pos.
      Limits packed value between 2^31-1 to -2^31

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 2^31-1 to -2^31
      @raise Bnary_pack_invalid_int_position
  *)
  val unpack_signed_32 : byte_order:endian -> buffer:t -> pos:int -> int32
  val unpack_signed_32_int : byte_order:endian -> buffer:t -> pos:int -> int

  (** Unpack signed integer data from given int32 or OCaml int, and
      write unpacked value to the buvver from the pos.
      Limits packed value between 2^31-1 to -2^31

      @param endian endianness of the buffer.
      @param buffer the buffer to write unpacked value.
      @param pos the position at the buffer to write unpack value.
      @param integer the integer value to unpack between 2^31-1 to -2^31
      @raise Bnary_pack_invalid_buffer_position
  *)
  val pack_signed_32 : byte_order:endian -> buffer:t -> pos:int -> int32 -> unit
  val pack_signed_32_int : byte_order:endian -> buffer:t -> pos:int -> int -> unit

  (** Pack signed 64bit integer data of the buffer at pos to int64.
      Use four element beginning of the buffer from pos.
      Limits packed value between 2^63-1 to -2^63

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 2^63-1 to -2^63
      @raise Bnary_pack_invalid_int64_position
  *)
  val unpack_signed_64 : byte_order:endian -> buffer:t -> pos:int -> int64

  (** Unpack signed integer data from given int64, and
      write unpacked value to the buvver from the pos.
      Limits packed value between 2^63-1 to -2^63

      @param endian endianness of the buffer.
      @param buffer the buffer to write unpacked value.
      @param pos the position at the buffer to write unpack value.
      @param integer the integer value to unpack between 2^63-1 to -2^63
      @raise Bnary_pack_invalid_buffer_position
  *)
  val pack_signed_64 : byte_order:endian -> buffer:t -> pos:int -> int64 -> unit

  (** Pack double precision floating number of the buffer at pos to float.
      Use eight element beginning of the buffer from pos.
      Limits packed value to ocaml float

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value between 2^63-1 to -2^63
      @raise Bnary_pack_invalid_float_position
  *)
  val unpack_float : byte_order:endian -> buffer:t -> pos:int -> float

  (** Unpack double precision floating number from given float, and
      write unpacked value to the buffer from the pos.
      Limits packed value to ocaml float

      @param endian endianness of the buffer.
      @param buffer the buffer to write unpacked value.
      @param pos the position at the buffer to write unpack value.
      @param integer the floating value to unpack
      @raise Bnary_pack_invalid_buffer_position
  *)
  val pack_float : byte_order:endian -> buffer:t -> pos:int -> float -> unit

  (** Pack uni precision floating number of the buffer at pos to float.
      Use four element beginning of the buffer from pos.
      Limits packed value to ocaml float

      @param endian endianness of the buffer.
      @param buffer the buffer to pack unsigned char.
      @param pos the position at the buffer to pack.
      @return packed value
      @raise Bnary_pack_invalid_float_position
  *)
  val unpack_float_c : byte_order:endian -> buffer:t -> pos:int -> float

  (** Unpack uni precision floating number from given float, and
      write unpacked value to the buffer from the pos.
      Limits packed value to ocaml float.

      Note: This function convert double precision floating number to uni,
      so unpacked value drop off some precision value from original.
      Therefore, there is not able to recover dropped off some precision.

      @param endian endianness of the buffer.
      @param buffer the buffer to write unpacked value.
      @param pos the position at the buffer to write unpack value.
      @param integer the floating value to unpack
      @raise Bnary_pack_invalid_buffer_position
  *)
  val pack_float_c : byte_order:endian -> buffer:t -> pos:int -> float -> unit

end

(** The functor make functions for given binary array. *)
module Make(A:BinaryArray) : S with type t = A.t

include S with type t = string
