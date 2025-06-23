-- Lifting a function with a constants as argument and result
-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output { [7i64, 7i64,10i64,16i64,25i64,37i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let bar (x : i64) (xs : []i64) : ([]i64, i64) =
  let ys = map (x*) xs
  in (ys, 7)

#[noinline]
let foo (x : i64) =
  let xs      = iota x
  let (ys, z) = bar 3 xs
  in z + reduce (+) 0 ys

def main (xs : []i64) = map foo xs

