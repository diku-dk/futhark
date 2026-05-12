-- Sumtypes in module types.
-- ==
-- input { }
-- output { 15 }

module type foobar_mod = {
  type^ foobar
  val f : foobar -> i32 -> i32
  val bar : foobar
}

module sum_module : foobar_mod = {
  type^ foobar = #foo i32 | #bar (i32 -> i32)

  def f (fb: foobar) (x: i32) : i32 =
    match fb
    case (#foo y) -> x + y
    case (#bar f) -> f x

  def bar = (#bar (+ 5)) : foobar
}

def main : i32 = sum_module.f sum_module.bar 10
