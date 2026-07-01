-- Lifting a function with a free variables as argument and result
-- ==
-- entry: main
-- input  { [  0i64,  1i64,  2i64,  3i64,  4i64,  5i64] }
-- output { [280i64,294i64,308i64,322i64,336i64,350i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
let v1 : []i64 = [5,9,6]

#[noinline]
let v2 : []i64 = [3,1,4,1,5]

#[noinline]
let bar (xs : []i64) (y : i64) : (i64, []i64) =
  let z = y + reduce (+) 0 xs
  in (z, copy v2)

#[noinline]
let foo (x : i64) =
  let (y, zs) = bar v1 x
  let z = reduce (+) 0 zs
  in (y * z)

def main (xs : []i64) = map foo xs
