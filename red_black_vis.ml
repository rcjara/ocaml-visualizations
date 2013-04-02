open Shared_types

module type SET =
sig
  type elem
  type struc
  val empty:      struc
  val insert:     elem -> struc -> struc
  val member:     elem -> struc -> bool
  val children:   struc -> struc list
  val label:      struc -> string
end

module type SET_FNCT =
  functor (Elem: ORDERED) -> SET with type elem = Elem.t

module RedBlackSet: SET_FNCT =
  functor (Elem: ORDERED) ->
    struct
      type elem  = Elem.t
      type color = R | B
      type tree  = E | T of color * tree * elem * tree
      type struc   = tree


      let empty = E

      let rec member x = function
        | E -> false
        | T (_, lft, y, rgt) ->
            if      Elem.lt x y then member x lft
            else if Elem.lt y x then member x rgt
            else true

      let balance = function
        | (B, T (R, T (R, a, x, b), y, c), z, d)
        | (B, T (R, a, x, T (R, b, y, c)), z, d)
        | (B, a, x, T (R, T (R, b, y, c), z, d))
        | (B, a, x, T (R, b, y, T (R, c, z, d))) ->
              T (R, T (B, a, x, b), y, T (B, c, z, d))
        | (c, a, x, b) -> T (c, a, x, b)

      let bal_lft = function
        | (B, T (R, T (R, a, x, b), y, c), z, d)
        | (B, T (R, a, x, T (R, b, y, c)), z, d) ->
              T (R, T (B, a, x, b), y, T (B, c, z, d))
        | (c, a, x, b) -> T (c, a, x, b)

      let bal_rgt = function
        | (B, a, x, T (R, T (R, b, y, c), z, d))
        | (B, a, x, T (R, b, y, T (R, c, z, d))) ->
              T (R, T (B, a, x, b), y, T (B, c, z, d))
        | (c, a, x, b) -> T (c, a, x, b)

      let insert x s =
        let rec ins = function
          | E -> T (R, E, x, E)
          | T (color, lft, y, rgt) as tree ->
              if Elem.lt x y then bal_lft (color, ins lft, y, rgt)
              else if Elem.lt y x then bal_rgt (color, lft, y, ins rgt)
              else tree
        in
        let T (_, lft, y, rgt) = ins s in
        T (B, lft, y, rgt)

      let children = function
        | E -> []
        | T (_, lft, _, rgt) -> [lft; rgt]

      let label  = function
        | E -> "E"
        | T (_, _, l, _) -> Elem.to_s l
    end

module RBS = RedBlackSet(OInt)


(*
let rec timed_draw _ =
    let rec delay _ =
      try
          Thread.delay 0.5
      with Unix.Unix_error (Unix.EAGAIN, _, _) -> delay ()
    in
    delay ();
    clear_graph ();
    a := RBS.insert (-1 * !i) !a;
    i := !i + 1;
    let (width, _) = RBS.dimensions !a in
    RBS.draw (400 - width / 2, 400) !a;
    if !i < 50
    then timed_draw ()
    else ()
*)
