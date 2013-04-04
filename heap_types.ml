exception EmptyHeap

module type Heap =
sig
  type heap
  type elem

  val empty:     heap
  val isEmpty:   heap -> bool

  val insert:    elem -> heap -> heap
  val merge:     heap -> heap -> heap

  val findMin:   heap -> elem
  val deleteMin: heap -> heap

  val fromList:  elem list -> heap
end
