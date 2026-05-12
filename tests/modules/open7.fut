-- #2251
-- ==
-- input { 1 } output { 124 }

module type mt1 = {
  val x : i32
}

module m1 : mt1 = {
  def x : i32 = 123
}

module m2 : mt1 = {
  open m1
}

entry main x = x + m2.x
