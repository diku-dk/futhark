-- Can we put type aliases in lambdas too?

type t = int
type ts = []t

fun main(xs: ts): []ts =
  map(fn (x: t): [10]t  => replicate 10 x,
      xs)
