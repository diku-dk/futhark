-- Parametricity says nothing about a *monomorphic* function in a module: this
-- "f" has type "arr [n] -> arr [n]", but "arr" is not one of its type
-- parameters, so for all we know it manufactures its result - as it does here.
-- ==
-- error: internal aliases

module M
  : {
      type arr [n]
      val mk : (n: i64) -> arr [n]
      val f [n] : arr [n] -> arr [n]
    } = {
  type arr [n] = ([n]i32, [n]i32)

  def mk n : arr [n] = let xs = replicate n 0 in (xs, xs)
  def f [n] (_x: arr [n]) : arr [n] = mk n
}

entry main [n] (x: M.arr [n]) : *M.arr [n] = M.f x
