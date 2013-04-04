type level = TOP | HEAD | RECURSIVE

module type Drawee =
sig
  type struc

  val to_level:  struc -> level
  val children:  struc -> struc list
  val label:     struc -> string
end

module type Drawer =
sig
  type struc
  val draw: int * int -> struc -> unit
  val dimensions: struc -> int * int
end
