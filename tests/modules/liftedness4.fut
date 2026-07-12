-- ==
-- error:

module PM (P: {type~ a val f : i64 -> a}) = {
  def gen (n: i64) = tabulate n P.f
}

module M = PM {type~ a = ?[k].[k]i64 def f = iota}

entry main = M.gen
