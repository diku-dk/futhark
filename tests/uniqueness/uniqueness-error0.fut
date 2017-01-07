-- Type ascription should not hide aliases.
-- ==
-- error:

fun main(): int =
  let a = iota(10)
  let b:*[]int = a
  let b[0] = 1
  in a[0]
