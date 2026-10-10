-- A global need not be a function itself to have function components that
-- manufacture abstract values.  Applying "ops.mk" still yields a value that may
-- have internal aliasing.
-- ==
-- error: internal aliases

module M
  : {
      type arr [n]
      val ops : {mk: (n: i64) -> arr [n]}
    } = {
  type arr [n] = ([n]i32, [n]i32)

  def ops = {mk = \(n: i64) -> let xs = replicate n 0 in (xs, xs)}
}

entry main n : *M.arr [] = M.ops.mk n
