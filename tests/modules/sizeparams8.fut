module meta
  : {
      val plus_comm [a] [b] 't : [a + b]t -> [b + a]t
    } = {
  def plus_comm [a] [b] 't (xs: [a + b]t) : [b + a]t = xs :> [b + a]t
}
