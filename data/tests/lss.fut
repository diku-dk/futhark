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
fun pred1(x: int): bool =
  True

fun pred2(x: int, y: int): bool =
  x <= y

fun max(x: int, y: int): int =
  if x > y then x else y

fun redOp(x: (int,int,int,int,int,int)) (y: (int,int,int,int,int,int)):
  (int,int,int,int,int,int) =
  let (lssx, lisx, lcsx, tlx, firstx, lastx) = x in
  let (lssy, lisy, lcsy, tly, firsty, lasty) = y in

  let connect = pred2(lastx, firsty) in
  let newlss = if connect then max(lcsx + lisy,
                                   max(lssx, lssy))
                          else max(lssx, lssy) in
  let newlis = if lisx == tlx && connect then lisx + lisy else lisx in
  let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy in
  let first = if tlx == 0 then firsty else firstx in
  let last  = if tly == 0 then lastx else lasty in

  (newlss, newlis, newlcs, tlx+tly, first, last)

fun mapOp (x: int): (int,int,int,int,int,int) =
  let xmatch = if pred1(x) then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

fun main(xs: []int): int =
  let (x,_,_,_,_,_) =
    reduce(redOp, (0,0,0,0,0,0), map(mapOp, xs)) in
  x
