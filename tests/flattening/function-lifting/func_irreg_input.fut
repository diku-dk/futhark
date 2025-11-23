-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output { [0i64, 0i64, 1i64, 3i64, 6i64,10i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let bar (xs : []i64) : i64 = reduce (+) 0 xs

#[noinline]
let foo (x : i64) =
  let xs = iota x
  in bar xs

def main (xs : []i64) = map foo xs
