-- A polymorphic value of an abstract type applied to the type parameter may
-- hold a global array, so it cannot be consumed.
-- ==
-- error: "pv", which is not consumable

module M : {
  type t 'a
  val mk 'a : t a
  val upd 'a : *t a -> *t a
} = {
  def global = [1i32, 2, 3]
  type t 'a = ([3]i32, [0]a)
  def mk 'a : t a = (global, [])
  def upd 'a ((xs, ys): *t a) : *t a = (xs with [0] = 42, ys)
}

def pv 'a : M.t a = M.mk

def main (_: i32) = M.upd (pv : M.t bool)
