module type withvec_mt = {
  val n : i64
  val xs : [n]i64
}

module withvec : withvec_mt = {
  def n = 3i64
  def xs = iota n
}
