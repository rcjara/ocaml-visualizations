open Shared_types

exception EmptyHeap

module type HEAP =
sig
  type heap
  type elem
  type struc


  val empty:     heap
  val isEmpty:   heap -> bool

  val insert:    elem -> heap -> heap
  val merge:     heap -> heap -> heap

  val findMin:   heap -> elem
  val deleteMin: heap -> heap

  val fromList:  elem list -> heap

  val children:  struc -> struc list
  val label:     struc -> string
end

module type HEAPFUNCTOR =
  functor (Elem: ORDERED) -> HEAP with type elem = Elem.t

module BH_Findmin: HEAPFUNCTOR =
  functor (Elem: ORDERED) ->
  struct
    type elem = Elem.t

    type tree =
      | E
      | Node of elem * tree list


    type b_tree = T of int * tree

    type heap = b_tree list

    let rank = function T (r, _) -> r
    let root = function T (_, Node (x, _)) -> x

    let empty = []
    let isEmpty = function
      | [] -> true
      | _ -> false

    let link (t1:b_tree) (t2:b_tree) :b_tree =
      match t1, t2 with
      | (T (r, (Node (x1, c1))), T (_, (Node (x2, c2)))) ->
          if Elem.lte x1 x2
          then (T (r + 1, Node (x1, (Node (x2, c2)) :: c1)))
          else (T (r + 1, Node (x2, (Node (x1, c1)) :: c2)))

    let rec insTree t = function
      | [] -> [t]
      | t' :: ts' as ts -> if rank t < rank t'
                           then t :: ts
                           else insTree (link t t') ts'

    let insert x ts = insTree (T (0, Node (x, []))) ts

    let rec merge ts1 ts2 =
      match ts1, ts2 with
      | ([], ts) | (ts, []) -> ts
      | ((t1 :: ts1'), (t2 :: ts2')) ->
          if rank t1 < rank t2      then t1 :: (merge ts1' ts2)
          else if rank t2 > rank t1 then t2 :: (merge ts1 ts2')
          else insTree (link t1 t2) (merge ts1' ts2')

    let rec removeMinTree = function
      | [t]     -> (t, [])
      | t :: ts ->
          let (t', ts') = removeMinTree ts in
            if root t < root t'
            then (t, ts)
            else (t', t :: ts')

    (* 3.6 *)
    let findMin ts =
      match ts with
      | [] -> raise EmptyHeap
      | _  ->
        let first_item::_ = ts in
        List.fold_left (fun a t -> if Elem.lte a (root t) then a else root t) (root first_item) ts

    let deleteMin ts =
      match ts with
      | [] -> raise EmptyHeap
      | _  -> let (T (r, Node (x, ts1)), ts2) = removeMinTree ts in
              merge (List.rev (List.map (fun t -> T (r - 1, t)) ts1)) ts2

    let fromList lst =
      List.fold_left (fun bh x -> insert x bh) empty lst

    (* Drawing code *)
    let children = function
    | (_ :: _) | [] as the_heap -> the_heap
    | T (_, ts) -> ts
    | (_, ts) -> ts

    let label = function
    | (_ :: _) | [] -> "HEAP"
    | T (o, _) -> "O" ^ (string_of_int o)
    | (e, _) -> Elem.to_s e


  end

module BH = BH_Findmin(OInt)
