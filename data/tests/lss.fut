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
fun bool pred1(int x) =
  True

fun bool pred2(int x, int y) =
  x <= y

fun int max(int x, int y) =
  if x > y then x else y

fun (int,int,int,int,int,int) redOp((int,int,int,int,int,int) x,
                                    (int,int,int,int,int,int) y) =
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

fun (int,int,int,int,int,int) mapOp (int x) =
  let xmatch = if pred1(x) then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

fun int main([int] xs) =
  let (x,_,_,_,_,_) =
    reduce(redOp, (0,0,0,0,0,0), map(mapOp, xs)) in
  x
