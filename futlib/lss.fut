-- Longest satisfying segment calculation.

module type LSS_PRED = {
  -- Element type.
  type t

  -- Doesn't have to be a neutral element, but must be _something_.
  val blank: t

  -- Does the single-element sequence satisfy?
  val pred1: t -> bool

  -- Does this two-element sequence satisfy?
  val pred2: t -> t -> bool
}

module LSS(P: LSS_PRED): { val lss: []P.t -> int } = {
  type t = P.t

  type slug = (int,int,int,int,t,t)

  fun max (x: int) (y: int) = if x < y then y else x

  fun redOp((lssx, lisx, lcsx, tlx, firstx, lastx): slug)
           ((lssy, lisy, lcsy, tly, firsty, lasty): slug): slug =

    let connect = P.pred2 lastx firsty
    let newlss = if connect then max (lcsx + lisy)
                                     (max lssx lssy)
                 else max lssx lssy
    let newlis = if lisx == tlx && connect then lisx + lisy else lisx
    let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy
    let first = if tlx == 0 then firsty else firstx
    let last  = if tly == 0 then lastx else lasty
    in (newlss, newlis, newlcs, tlx+tly, first, last)

  fun mapOp (x: t): slug =
    let xmatch = if P.pred1 x then 1 else 0
    in (xmatch, xmatch, xmatch, 1, x, x)

  fun lss(xs: []t): int =
    let (x,_,_,_,_,_) =
      reduce redOp (0,0,0,0,P.blank,P.blank) (map mapOp xs)
    in x
}
