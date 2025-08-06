module type thing = {
  val n : i64
  val f : i64 -> [n / 2]i64
}

module mk_m (G: thing) = {
  def lex [m] (i: [m]i64) : [m][G.n / 2]i64 =
    map G.f i
}

module m = mk_m {
  def n : i64 = 2
  def f i = replicate (n / 2) i
}

def x = m.lex [1, 2, 3]

-- ==
-- entry: test
-- random input { } output { 6i64 }
entry test =
  flatten x |> i64.sum
