-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- auto output
-- input  { empty([0]i64) }
-- auto output

#[noinline]
def bar (xs: []i64) : i64 = reduce (+) 0 xs

#[noinline]
def foo (x: i64) =
  let xs = replicate 5 x
  in bar xs

def main (xs: []i64) = map foo xs
