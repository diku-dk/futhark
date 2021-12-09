def foo x =
  #[sequential]
  let n = 2i64
  let ys = replicate n 0
  let ys[0] = x
  let bar = all (== 0i64) ys
  let baz = all (\i -> ys[i] == 0) (0..<n)
  in bar && baz

def main xs = map foo xs
