-- Lifting a function with an irregular
-- parameter and return type
-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64,  4i64, 5i64] }
-- output { [0i64, 0i64, 0i64, 3i64, 15i64,45i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let bar (xs : []i64) : []i64 =
  let y = reduce (+) 0 xs
  in iota y

#[noinline]
let foo (x : i64) =
  let xs = iota x
  let ys = bar xs
  in reduce (+) 0 ys

def main (xs : []i64) = map foo xs

