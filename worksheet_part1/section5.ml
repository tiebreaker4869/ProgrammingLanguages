module type NONEGINT = sig
  type t
  val mknni: int -> t option
  val add: t->t->t
  val mul: t->t->t
  val sub: t->t->t option
  val to_int: t->int
end


module NonNegInt : NONEGINT = struct
  type t = int

  let mknni i =
    if i < 0 then
      None
    else
      Some i

  let add a b =
    a + b

  let mul a b = 
    a * b (* why not mknni (a * b) ? *)

  let sub a b =
    mknni (a - b) (* why not a - b ? *)


  (* external world doesn't know this "is it" *)
  let to_int a = a
end

module type CIRC = sig
  type circle
  val unitCircle: circle
  val moveX: circle * float -> circle
  val moveY: circle * float -> circle
  val scaleR: circle * float -> circle
  val intersect: circle * circle -> bool
  val area: circle -> float
  val funnyCircleMake: float -> circle
end

module Circ : CIRC = struct
  type circle = float * float * float
  (* center x-coordinate, center y-coordinate, radius *)

  let unitCircle = (0.0, 0.0, 1.0)
  (* center at origin, radius 1 *)

  let moveX ((x,y,r), dx) = (x +. dx, y, r)
  let moveY ((x,y,r), dy) = (x, y +. dy, r)
  let scaleR ((x,y,r), f) = (x, y, r *. f)
  let intersect ((x1,y1,r1),(x2,y2,r2)) = let dx = x2 -. x1 in
  let dy = y2 -. y1 in
sqrt(dx*.dx +. dy*.dy) < r1 +. r2
  let area (_, _, r) = Float.pi *. r *. r  
  let funnyCircleMake a = (0.0, 0.0, a)
end

module type MINMAXLIST = sig
  type my_int_list
  val new_ : int -> my_int_list
  val add : int * my_int_list -> my_int_list
  val max : my_int_list -> int
  val min : my_int_list -> int
end

module MinMaxList : MINMAXLIST = struct
  type my_int_list = int list

  exception Bad

  let empty = []

  let cons (i,xs) = i::xs

  let new_ i = i::[]

  let add (i,xs) =
    match xs with
    | [] -> i::[]
    | j::ys -> if i < j then j::i::ys else i::xs

  let rec min xs =
    match xs with
    | [] -> raise Bad
    | i::[] -> i
    | i::ys -> let m = min ys in if i < m then i else m

  let max xs =
    match xs with
    | [] -> raise Bad
    | i::_ -> i

end
