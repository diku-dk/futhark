-- Can we put type aliases in lambdas too?

type t = int
type ts = [t]

fun [ts] main(ts xs) =
  map(fn [t,10] (t x) => replicate(10, x),
      xs)
