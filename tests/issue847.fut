-- Tiling bug.

def main (acc: []i64) (c: i64) (n: i64) =
  let is = map (+ c) (iota n)
  let fs = map (\i -> reduce (+) 0 (map (+ (i + c)) acc)) (iota n)
  in (fs, is)
