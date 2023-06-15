-- Lifting a function with both regular and irregular
-- parameters and return types.
-- ==
-- entry: main
-- input  { [0i64, 1i64,  2i64,  3i64,  4i64,     5i64] [0i64, 1i64, 2i64, 3i64,  4i64,  5i64] }
-- output { [0i64, 0i64, -1i64, 27i64, 252i64, 1175i64] }
-- input  { [5i64, 4i64, 3i64, 2i64, 1i64, 0i64] [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output { [0i64, 9i64, 9i64, 0i64, 0i64, 0i64] }
-- input  { empty([0]i64) empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let bar (y : i64) (xs : []i64) : ([]i64, i64) =
  let z = y * reduce (+) 0 xs
  in (iota z, z)

#[noinline]
let foo (a : i64) (b : i64) =
  let xs      = iota a
  let (ys, z) = bar b xs
  in reduce (+) 0 ys - z

def main (as : []i64) (bs : []i64) = map2 foo as bs

