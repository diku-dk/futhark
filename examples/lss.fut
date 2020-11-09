-- Parallel longest satisfying segment
--
-- Written as a function that is parameterisered over the satisfaction
-- property.  Cannot handle empty input arrays.
--
-- ==
-- input { [1, -2, -2, 3, 4, -1, 5, -6, 1] }
-- output { 4 }
-- input { [5, 4, 3, 2, 1] }
-- output { 1 }
-- input { [1, 2, 3, 4, 5] }
-- output { 5 }

-- The two relations must describe a transitive relation.
let lss [n] 't (pred1: t -> bool) (pred2: t -> t -> bool) (xs: [n]t) =
  let max = i32.max

  let redOp (lssx, lisx, lcsx, tlx, firstx, lastx)
            (lssy, lisy, lcsy, tly, firsty, lasty) =
    let connect = pred2 lastx firsty || tlx == 0 || tly == 0
    let newlss = if connect then max (lcsx + lisy) (max lssx lssy)
                            else max lssx lssy
    let newlis = if lisx == tlx && connect then lisx + lisy else lisx
    let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy
    let first = if tlx == 0 then firsty else firstx
    let last  = if tly == 0 then lastx else lasty
    in (newlss, newlis, newlcs, tlx+tly, first, last)

    let mapOp x =
      let xmatch = if pred1 x then 1 else 0
      in (xmatch, xmatch, xmatch, 1, x, x)

  in (reduce redOp (0,0,0,0,xs[0],xs[0]) (map mapOp xs)).0

let main (xs: []i32): i32 =
  lss (\_ -> true) (\(x: i32) y -> x <= y) xs
