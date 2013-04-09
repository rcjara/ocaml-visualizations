include Shared_types

exception EmptyHeap

module type Heap =
sig
  type heap
  type t

  val empty:     heap
  val isEmpty:   heap -> bool

  val insert:    t -> heap -> heap
  val merge:     heap -> heap -> heap

  val findMin:   heap -> t
  val deleteMin: heap -> heap

  val fromList:  t list -> heap
end

module type HeapFnct =
  functor (M: Ord) -> Heap with type t = M.t
