-- Can we put type aliases in lambdas too?

type t = i32
type ts [n] = [n]t

def main (xs: ts []) : [](ts []) =
  map (\(x: t) : [10]t -> replicate 10 x) xs
