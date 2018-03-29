-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 10 }

import "/futlib/math"
import "/futlib/mss"

let mss_int = mss' id

let mss_weird (xs: []i32) =
  let as_int (x: i32, xlen) = x + xlen
  let max x y = if as_int x < as_int y then y else x
  let combine (x: i32, xlen: i32) (y, ylen) = (x+y, xlen+ylen)
  in mss (0,0) max combine (\x -> (x,1)) xs

let main(xs: []i32) =
  (mss_int xs,
   (mss_weird xs).1)
