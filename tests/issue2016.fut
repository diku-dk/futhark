-- ==
-- error: non-constructively

module trouble
  : {
      type const 'a 'b
      val mk_const 'a 'b : a -> b -> const a b
    } = {
  type const 'a 'b = a
  def mk_const x _ = x
}

entry f [n] [m] (_: trouble.const ([n]i64) ([m]i64)) =
  m

entry g (x: i64) =
  let [n][m] (_: trouble.const ([n]i64) ([m]i64)) = trouble.mk_const (iota (x + 1)) (iota x)
  in m
