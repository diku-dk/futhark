-- A polymorphic value of an abstract type applied to the type parameter may
-- have internal aliasing, so it cannot be consumed.
-- ==
-- error: "pv", which is not consumable

module M : {
  type t 'a
  val mk 'a : i64 -> t a
  val upd 'a : *t a -> *t a
} = {
  type t 'a = ([3]i32, [3]i32, [0]a)
  def mk 'a (_: i64) : t a = let xs = map i32.i64 (iota 3) in (xs, xs, [])
  def upd 'a ((xs, ys, zs): *t a) : *t a = (xs with [0] = 42, ys, zs)
}

def pv 'a : M.t a = M.mk 3

def main (_: i32) = M.upd (pv : M.t bool)
