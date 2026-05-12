def f (xs: *[][]bool) =
  #[unsafe]
  let a = xs[0]
  let b = copy a
  let xs[0, 1] = true
  in (b[0], xs[0, 0])

def main A = map (\xs -> f (copy xs)) A
