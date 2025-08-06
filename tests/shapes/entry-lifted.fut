module m
  : {
      type~ t
      val mk : i64 -> *t
    } = {
  type~ t = #foo ([]i32) | #bar ([]i32)
  def mk (n: i64) : *t = #foo (replicate n 0)
}

def main n = m.mk n
