-- ==
-- input { [true,true,false,true,false] [1i64,-1,2,3,4] [1i64,2,3,4,5] }
-- auto output
-- input { [true,true,true,true,true] [1i64,-1,2,3,4] [1i64,2,3,4,5] }
-- auto output
import "../../accs/intrinsics"


def update [n] (acc: *acc ([n]i64)) (flag: bool, i: i64, x: i64)
    : acc ([n]i64) =
 
  loop  acc = acc for j < 10 do
  let z = replicate 10 x with [2] = j in
  let res = i64.sum z in
  write acc i res
entry main [n] (flags: [n]bool) (is: [n]i64) (xs: [n]i64) =
  scatter_stream (replicate n 0) update (zip3 flags is xs)
