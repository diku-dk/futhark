-- The result of a polymorphic function whose type is an array of an abstract
-- type applied to the type parameter may be a global array, so it cannot be
-- consumed.
-- ==
-- error: "f", which is not consumable

module M : {
  type t 'a
  val arr 'a : [3](t a)
  val upd 'a : *[3](t a) -> *[3](t a)
} = {
  def global = [1i32, 2, 3]
  type t 'a = i32
  def arr 'a : [3](t a) = global
  def upd 'a (xs: *[3](t a)) : *[3](t a) = xs with [0] = 42
}

def f 'a (_: a) : [3](M.t a) = M.arr

def main (_: i32) = M.upd (f true)
