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


module type SET =
sig
  type elem
  type set
  val empty:      set
  val insert:     elem -> set -> set
  val member:     elem -> set -> bool
  val dimensions: set -> int * int
  val draw:       int * int -> set -> unit
end

module type SET_FNCT =
  functor (Elem: ORDERED) -> SET with type elem = Elem.t

module RedBlackSet: SET_FNCT =
  functor (Elem: ORDERED) ->
    struct
      type elem  = Elem.t
      type color = R | B
      type tree  = E | T of color * tree * elem * tree
      type set   = tree

      type pt     = (int * int)
      type dim    = (int * int)
      type bounds = (pt * pt)

      let radius = 10
      let diam   = 2 * radius

      let empty = E

      let rec member x = function
        | E -> false
        | T (_, lft, y, rgt) ->
            if      Elem.lt x y then member x lft
            else if Elem.lt y x then member x rgt
            else true

      let rec dimensions = function
        | E -> (0, 0)
        | T (_, lft, _, rgt) ->
            match dimensions lft, dimensions rgt with
            | ((w1, h1), (w2, h2)) ->
              let max_h = max h1 h2
              and cum_w = w1 + w2
              in if max_h = 0
                 then (diam, diam)
                 else (diam + cum_w, diam + radius + max_h)

      let draw_between (x1, y1) (x2, y2) off =
        let x1' = float_of_int x1
        and y1' = float_of_int y1
        and x2' = float_of_int x2
        and y2' = float_of_int y2
          in
        let dist = sqrt ((x2' -. x1') ** 2. +. (y2' -. y1') ** 2.)
        and ang  = atan2 (y2' -. y1') (x2' -. x1')
          in
        let ex1' = x1' +. off *. (cos ang)
        and ey1' = y1' +. off *. (sin ang)
        and ex2' = x1' +. (dist -. off) *. (cos ang)
        and ey2' = y1' +. (dist -. off) *. (sin ang)
          in
        let ex1 = int_of_float ex1'
        and ey1 = int_of_float ey1'
        and ex2 = int_of_float ex2'
        and ey2 = int_of_float ey2'
          in
        moveto ex1 ey1;
        lineto ex2 ey2


      let draw_elem (x, y) (T (_, _, e, _)) =
        let str = Elem.to_s e in
        let (w, h) = text_size str in
        moveto (x - (w / 2) + 1) (y - h / 2);
        draw_string str

      let center x y = function
        | E -> None
        | T _ as node ->
          let (w, h) = dimensions node in
          Some (x + (w - diam) / 2 + radius, y - radius)

      let node_color (T (c, _, _, _)) =
        if c = R
        then set_color red
        else set_color black

      let rec draw (x, y) = function
        | E -> ()
        | T (_, lft, _, rgt) as node ->
            let Some (ctr_x, ctr_y) = center x y node
            and (lft_w, _)          = dimensions lft
            and (rgt_w, _)          = dimensions rgt
            and top_offset          = diam + radius in
            let lft_x               = x
            and rgt_x               = x + lft_w + diam in
            let connect_to_child (x, y) = function
              | E -> ()
              | T _ as child ->
                match center x y child with
                | None -> ()
                | Some (chd_x, chd_y) ->
                    node_color child;
                    draw_between (ctr_x, ctr_y) (chd_x, chd_y) (float_of_int (radius + 1))
            in
              set_color black;
              set_line_width 1;
              draw_elem (ctr_x, ctr_y) node;
              draw_circle ctr_x ctr_y radius;
              set_line_width 2;
              connect_to_child (lft_x, y - top_offset) lft;
              connect_to_child (rgt_x, y - top_offset) rgt;
              draw (lft_x, y - top_offset) lft;
              draw (rgt_x, y - top_offset) rgt


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
    end

module RBS = RedBlackSet(OInt)

let a = ref RBS.empty
let i = ref 1

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

