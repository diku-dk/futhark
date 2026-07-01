-- ==
-- entry: main
-- nobench input  { [0i64, 0i64, 0i64, 1i64, 1i64, 1i64, 2i64, 2i64, 2i64] [0i64, 1i64, 2i64, 0i64, 1i64, 2i64, 0i64, 1i64, 2i64] }
-- output         { [7i64, -5i64, -4i64, 2i64, -1i64, -1i64, 1i64, -1i64, 2i64] }
-- nobench input  { [0i64, 0i64, 0i64, 1i64, 1i64, 1i64, 2i64, 2i64, 2i64] [2i64, 2i64, 2i64, 1i64, 1i64, 1i64, 0i64, 0i64, 0i64] }
-- output         { [-4i64, -4i64, -4i64, -1i64, -1i64, -1i64, 1i64, 1i64, 1i64] }
-- nobench input  { [1i64, 2i64, 3i64] [4i64, 5i64, 6i64] }
-- output         { [2i64, 35i64, 135i64] }
-- nobench input  { empty([0]i64) empty([0]i64) }
-- output         { empty([0]i64) }

#[noinline]
let foo (x : i64) (y : i64) (zs : []i64) =
  let (a, as) = 
    match (x, y)
    case (0,0) -> (3,iota 5)
    case (0,b) -> (5,iota b)
    case (a,0) -> (a,iota 3)
    case (a,b) -> (a*b, zs)
  in reduce (+) 0 as - a

let bar (x : i64) (y : i64) =
  let zs = iota (x * y) in foo x y zs

def main [n] (xs : [n]i64) (ys : [n]i64) = map2 bar xs ys
