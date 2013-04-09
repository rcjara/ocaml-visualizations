module type Ord =
sig
  type t

  val to_s: t -> string
  val cmp:  t -> t -> int
  val base: t
end

module type Ordered = sig
  include Ord

  val lt:      t -> t -> bool
  val lte:     t -> t -> bool
  val eq:      t -> t -> bool
  val gte:     t -> t -> bool
  val gt:      t -> t -> bool
end

module type OrderedFnct =
  functor (M : Ord) -> Ordered with type t = M.t

module Ordered : OrderedFnct =
  functor (M : Ord) -> struct

  include M

  let lt  a b = M.cmp a b = -1
  let lte a b = M.cmp a b <= 0
  let eq  a b = M.cmp a b =  0
  let gte a b = M.cmp a b >= 0
  let gt  a b = M.cmp a b =  1
end

module MyInt = struct
  type t = int

  let to_s = string_of_int
  let cmp = compare
  let base = 1
end

module OInt = Ordered(MyInt)
