-- ==
-- input { 4 } output { [3] }

def ilog2 (x: i32) = 31 - i32.clz x

def main (n: i32) =
  let m = ilog2 n
  let id = 1 << (m - 1)
  let indexes = id - 1..id * 2 - 1...n - 1
  in indexes[1:]
