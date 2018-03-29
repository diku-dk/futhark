-- Longest satisfying segments.

let lss 't (blank: t) (pred1: t -> bool) (pred2: t -> t -> bool) (ts: []t): i32 =
  let redop (lssx, lisx, lcsx, tlx, firstx, lastx)
            (lssy, lisy, lcsy, tly, firsty, lasty) =
        let connect = pred2 lastx firsty
        let newlss = if connect then i32.max (lcsx + lisy)
                                             (i32.max lssx lssy)
                     else i32.max lssx lssy
        let newlis = if lisx == tlx && connect then lisx + lisy else lisx
        let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy
        let first = if tlx == 0 then firsty else firstx
        let last  = if tly == 0 then lastx else lasty
        in (newlss, newlis, newlcs, tlx+tly, first, last)
  let mapop x =
        let xmatch = if pred1 x then 1 else 0
        in (xmatch, xmatch, xmatch, 1, x, x)
  in (reduce redop (0,0,0,0,blank,blank) (map mapop ts)).1
