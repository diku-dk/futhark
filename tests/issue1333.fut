-- ==
-- input {true} output { 1i64 }

module type mt = {
  type arr [n]
  val mk : bool -> arr []
}

module m : mt = {
  type arr [n] = [n]bool
  def mk b = [b]
}

def main b =
  let f [n] (_: m.arr [n]) = n
  in f (m.mk b)
