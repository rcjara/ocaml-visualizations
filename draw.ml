open Graphics
include Multi_level_types
include Multi_level_bi_heap



module type DrawFnct =
  functor (ToDraw: Drawee) -> Drawer with type struc = ToDraw.struc

module Drawer: DrawFnct =
  functor (ToDraw: Drawee) ->
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

    let center x y (w, h) =
      (x + (w - diam) / 2 + radius, y - radius)

    let rec draw (x, y) (node: struc) =
      let dim = dimensions node in
      let (w, _) = dim in
      let (ctr_x, ctr_y) = center x y dim
      and top_offset     = diam + (w / 6) in
      let rec connect_to_child (x, y) child =
        let (chd_x, chd_y) = center x y (dimensions child) in
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

module BH_Drawer = Drawer(DBH)

let a = ref DBH.empty
let i = ref 0

let size = 800

let draw _ =
  clear_graph ();
  let drawable = (DBH.to_struc !a) in
  let (width, _) = BH_Drawer.dimensions drawable in
  BH_Drawer.draw ((size - width) / 2, size - 20) drawable

let draw_struct s =
  clear_graph ();
  let (width, _) = BH_Drawer.dimensions s in
  BH_Drawer.draw ((size - width) / 2, size - 20) s

let insert_and_draw x =
  a := DBH.insert x !a;
  draw ()

let inc_insert _ =
  a := DBH.insert !i !a;
  i := !i + 1;
  draw ()

let rand_insert _ =
  a := DBH.insert (Random.int 100) !a;
  draw ()

let delete_and_draw _ =
  a := DBH.deleteMin !a;
  draw ()

let o0 = DBH.binomial_tree 0 1
let o1 = DBH.binomial_tree 1 2
let o2 = DBH.binomial_tree 2 3
let o3a = DBH.binomial_tree 3 0
let o3b = DBH.binomial_tree 3 1

let setup _ =
  let str_size = (string_of_int size) in
  open_graph (" " ^ str_size ^ "x" ^ str_size);
  draw_struct o0

