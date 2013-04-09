open Shared_types
open Heap_types
open Multi_level_types
open Binomial_heap

module type DrawableHeap =
sig
  include Heap
  include Drawee

  val to_struc: heap -> struc
  val binomial_tree: int -> t -> struc
  val link_tree: struc -> struc -> struc
end

module type DrawableHeapFnct =
  functor (Elem: Ord) -> DrawableHeap with type t = Elem.t

module DrawableBiHeap (Elem: Ord) =
struct
    include BinomialHeap(Elem)
    (* Drawing code *)
    type struc =
      | Top       of heap
      | Head      of b_tree
      | Recursive of tree

    let to_struc heap = Top heap

    let children = function
    | Top b_ts ->
        List.map (fun b_t -> Head b_t) b_ts
    | Head (T (_, t)) -> [Recursive t]
    | Recursive (Node (_, ts)) ->
        List.map (fun t -> Recursive t) ts
    | Recursive E -> []

    let label = function
    | Top _ -> "RT"
    | Head (T (o, _)) ->
        let f = float_of_int o in
        let size = 2. ** f in
        "S" ^ (string_of_int (int_of_float size))
    | Recursive (Node (e, _)) -> Elem.to_s e
    | Recursive E -> "E"

    let to_level = function
    | Top _ -> TOP
    | Head _ -> HEAD
    | Recursive _ -> RECURSIVE

    let binomial_tree o item =
      let rec range i j = if i > j then [] else item :: (range (i+1) j) in
      let num_elements = int_of_float (2.0 ** (float_of_int o)) in
      let heap = fromList (range 1 num_elements) in
      let tree::_ = heap in
      Head tree

    let link_tree (Head t1) (Head t2) = Head (link t1 t2)
  end


module BH = BinomialHeap(MyInt)
module DBH = DrawableBiHeap(MyInt)

