-- Does it work to have a definition that only has a size parameter?
-- ==
-- input { 3i64 } output { [0i64,1i64,2i64] }

module m
  : {
      val iota [n] : [n]i64
    } = {
  def iota [n] : [n]i64 = 0..1..<n
}

def main x = copy m.iota : [x]i64
