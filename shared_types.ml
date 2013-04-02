module type ORD =
sig
  type t

  val to_s: t -> string
  val cmp:  t -> t -> int
end

module type ORDERED =
sig
  type t

  val to_s:    t -> string

  val cmp:     t -> t -> int
  val lt:      t -> t -> bool
  val lte:     t -> t -> bool
  val eq:      t -> t -> bool
  val gte:     t -> t -> bool
  val gt:      t -> t -> bool
end

module type ORDERED_FNCT =
  functor (Elem: ORD) -> ORDERED with type t = Elem.t

module Ordered =
  functor (Elem: ORD) ->
  struct
    type t = Elem.t

    let to_s = Elem.to_s

    let cmp  = Elem.cmp

    let lt  a b = cmp a b = -1
    let lte a b = cmp a b <= 0
    let eq  a b = cmp a b =  0
    let gte a b = cmp a b >= 0
    let gt  a b = cmp a b =  1
  end

module MyInt: ORD with type t = int =
struct
  type t = int

  let to_s = string_of_int
  let cmp = compare
end

module OInt = Ordered(MyInt)

