-- Abstract types should be treated conservatively for uniqueness inference.
-- The function mk returns two aliased arrays, so the result cannot be unique.
-- ==
-- input { 5i64 }
-- output { [0i32, 0i32, 0i32, 0i32, 0i32] [0i32, 0i32, 0i32, 0i32, 0i32] }

module M
  : {
      type arr [n]
      val mk : (n: i64) -> arr [n]
    } = {
  type arr [n] = ([n]i32, [n]i32)

  def mk n : arr [n] = let xs = replicate n 0 in (xs, xs)
}

entry main n = M.mk n
