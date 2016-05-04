-- This program broke the simplifier at one point.

fun [int] main(int x, int y, [int] a, [int] b) =
  let c = map(fn (int,int) (int av) =>
                let v = x + y in
                (v, 2*av),
              a) in
  map(+, c)
