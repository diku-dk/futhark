-- ==
-- input { 2 } output { 2 }

module type mt = {
  type^ abs
  val mk : i32 -> abs
  val len : abs -> i32
}

module m : mt = {
  type~ abs = []i64
  def mk (n: i32) = iota (i64.i32 n)
  def len [n] (_: [n]i64) = i32.i64 n
}

def main (x: i32) = m.len (m.mk x)
