-- Trying to sneak more constraints through a size-lifted type!
-- ==
-- error: val f

module type mt = {
  type~ t
  val f : t -> i32
}

module m : mt = {
  type~ t = [][]i32
  let f [n] (_: [n][n]i32) = n
}
