-- A function that returns a non-unique abstract type may be returning a value
-- that aliases itself - here, a pair of arrays that are really the same array.
-- Such a value must not be given a unique type.  See issue #2531.
-- ==
-- error: internal aliases

module M
  : {
      type arr [n]
      val mk : (n: i64) -> arr [n]
    } = {
  type arr [n] = ([n]i32, [n]i32)

  def mk n : arr [n] = let xs = replicate n 0 in (xs, xs)
}

entry main n : *M.arr [] = M.mk n
