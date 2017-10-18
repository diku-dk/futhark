-- Parallel longest satisfying segment
--
-- Since Futhark does not support higher-order functions, we try to
-- find the longest sorted sequence.
-- ==
-- input { [1, -2, -2, 3, 4, -1, 5, -6, 1] }
-- output { 4 }
-- input { [5, 4, 3, 2, 1] }
-- output { 1 }
-- input { [1, 2, 3, 4, 5] }
-- output { 5 }

-- These two functions define the satisfaction property.  pred2 must
-- be transitive.
let pred1(x: i32): bool =
  true

let pred2(x: i32, y: i32): bool =
  x <= y

let max(x: i32, y: i32): i32 =
  if x > y then x else y

let redOp(x: (i32,i32,i32,i32,i32,i32)) (y: (i32,i32,i32,i32,i32,i32)):
  (i32,i32,i32,i32,i32,i32) =
  let (lssx, lisx, lcsx, tlx, firstx, lastx) = x
  let (lssy, lisy, lcsy, tly, firsty, lasty) = y

  let connect = pred2(lastx, firsty)
  let newlss = if connect then max(lcsx + lisy,
                                   max(lssx, lssy))
                          else max(lssx, lssy)
  let newlis = if lisx == tlx && connect then lisx + lisy else lisx
  let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy
  let first = if tlx == 0 then firsty else firstx
  let last  = if tly == 0 then lastx else lasty in

  (newlss, newlis, newlcs, tlx+tly, first, last)

let mapOp (x: i32): (i32,i32,i32,i32,i32,i32) =
  let xmatch = if pred1(x) then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

let main(xs: []i32): i32 =
  let (x,_,_,_,_,_) =
    reduce redOp (0,0,0,0,0,0) (map mapOp xs) in
  x
