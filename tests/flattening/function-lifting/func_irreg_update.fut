-- Lifting a function which consumes its argument
-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output { [0i64, 0i64, 0i64, 1i64, 2i64, 4i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let bar [n] (xs : *[n]i64) (z : i64) (ys : [z]i64) : [n]i64 =
  let m = n - z
  in xs with [m:n] = ys

#[noinline]
let foo (a : i64) =
  let b = a / 2
  let xs = iota a
  let ys = iota b :> [b]i64
  let zs = bar xs b ys
  in reduce (+) 0 zs

def main (xs : []i64) = map foo xs
