open Graphics
open Red_black_vis

module type DRAWEE =
sig
  type struc
  val children: struc -> struc list
  val label:    struc -> string
end

module type DRAWER =
sig
  type struc
  val draw: int * int -> struc -> unit
  val dimensions: struc -> int * int
end


module type DRAW_FNCT =
  functor (ToDraw: DRAWEE) -> DRAWER with type struc = ToDraw.struc

module Drawer: DRAW_FNCT =
  functor (ToDraw: DRAWEE) ->
  struct
    type struc = ToDraw.struc

    type pt    = (int * int)
    type dim   = (int * int)

    let radius = 12
    let diam   = 2 * radius

    let rec dimensions structure =
      let children = ToDraw.children structure in
      let children_dims = List.map dimensions children
      and reduce_fn = (fun (w1, h1) (w2, h2) -> (w1 + w2, max h1 h2)) in
      let (w', h') = List.fold_left reduce_fn (0, 0) children_dims
      and horiz_padding = radius * (max ((List.length children) - 1) 0) in
      ((max w' 20) + horiz_padding, h' + 20)

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


    let draw_elem (x, y) e =
      let str = ToDraw.label e in
      let (w, h) = text_size str in
      moveto (x - (w / 2) + 1) (y - h / 2);
      draw_string str

    let center x y node =
      let (w, h) = dimensions node in
      (x + (w - diam) / 2 + radius, y - radius)

    let rec draw (x, y) (node: struc) =
      let (ctr_x, ctr_y) = center x y node
      and top_offset     = diam + radius in
      let rec connect_to_child (x, y) child =
        let (chd_x, chd_y) = center x y child in
          draw_between (ctr_x, ctr_y) (chd_x, chd_y) (float_of_int (radius + 1))
      and draw_children (x, y) node =
        let (w, _) = dimensions node in
        connect_to_child (x, y) node;
        draw (x, y) node;
        (x + w + radius, y)
      in
        set_color black;
        set_line_width 1;
        draw_elem (ctr_x, ctr_y) node;
        draw_circle ctr_x ctr_y radius;
        List.fold_left draw_children (x, y - top_offset) (ToDraw.children node);
        ()
  end

module RB_Drawer = Drawer(RBS)

let a = ref RBS.empty
let i = ref 1

let insert_and_draw x =
  a := RBS.insert x !a;
  clear_graph ();
  let (width, _) = RB_Drawer.dimensions !a in
  RB_Drawer.draw (300 - width / 2, 300) !a
