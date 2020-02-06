module type withvec_mt = {
  val n : i32
  val xs : [n]i32
}

module withvec : withvec_mt = {
  let n = 3i32
  let xs = iota n
}
