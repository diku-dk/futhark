module type mod_b = {
  type t
  val n : i64
  val f : [n]t -> t
}

module type c = {
  module B: mod_b
}

module make_c (B: mod_b) : c = {
  module B = B
}
