-- Type-parametric type in module type.
-- ==
-- input { 1 2 } output { [1,2] }

module type MT = {
  type t 'a
  val pack [n] : [n]i32 -> t i32
  val unpack : t i32 -> []i32
}

module M0 : MT = {
  type t 'a = (a, a)
  def pack (xs: []i32) = (xs[0], xs[1])
  def unpack (x: i32, y: i32) = [x, y]
}

module M1 : MT = {
  type t 'a = [2]a
  def pack (xs: []i32) = [xs[0], xs[1]]
  def unpack (xs: t i32) = xs
}

def main (x: i32) (y: i32) : []i32 =
  let a: M0.t i32 = M0.pack [x, y]
  let b: M1.t i32 = M1.pack (M0.unpack a)
  in M1.unpack b
