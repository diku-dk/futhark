type t = i32
type ts [n] = [n]t

def main (xs: ts [], x: t) : ts [] =
  map (+ x) xs
