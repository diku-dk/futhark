module type withvec_mt = {
  val n : i64
  val xs : [n]i64
}

module withvec : withvec_mt = {
  let n = 3i64
  let xs = iota n
}
