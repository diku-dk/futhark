-- ==
-- error: anonymous-nonconstructive

module m2
  : {
      type^ t
      val x : bool -> t
      val f : t -> i64
    } = {
  type^ t = []bool -> bool
  def x b = \(_: [10]bool) : bool -> b
  def f [n] (_: [n]bool -> bool) = n
}

entry main2 = m2.f (m2.x true)
