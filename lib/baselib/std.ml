module Collection = Collection
module CompareStringable = CompareStringable
module Math = Math
module Monad = Monad
module Option = Option
module Prelude = Prelude
module Stringable = Stringable
module TypedCollection = TypedCollection
module TypedStringable = TypedStringable
module Binary_pack = Binary_pack
module Input_channel = Input_channel

module Either = Either

module Comparable = Comparable

(* extended List *)
module List = struct
  (* Inherits the original List module *)
  include List

  (* Inherits my extended List *)
  include Ext_list
end

module Array = struct
  (* Inherits the original Array module *)
  include Array

  (* Inherits my extended Array *)
  include Ext_array
end

(* Original Map module *)
module Map = Ext_map
