-- ==
-- input { 2 } output { 2 }

module type mt = {
  type^ abs
  val mk : i32 -> abs
  val len : abs -> i32
}

module m : mt = {
  type^ abs = bool -> i32
  def mk (n: i32) = \_ -> n
  def len (f: abs) = f true
}

def main (x: i32) = m.len (m.mk x)
