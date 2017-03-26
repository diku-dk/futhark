-- Maximum segment sum with a custom measure.

module type MSS_MEASURE = {
  type t
  type m

  val zero: m
  val max: m -> m -> m
  val combine: m -> m -> m

  val single: t -> m
}

module MSS(M: MSS_MEASURE): { val mss: []M.t -> M.m } = {

  let redOp((mssx, misx, mcsx, tsx): (M.m,M.m,M.m,M.m))
         ((mssy, misy, mcsy, tsy): (M.m,M.m,M.m,M.m)): (M.m,M.m,M.m,M.m) =
    ( M.max mssx (M.max mssy (M.combine mcsx misy))
    , M.max misx (M.combine tsx misy)
    , M.max mcsy (M.combine mcsx tsy)
    , M.combine tsx tsy)

  let mapOp (x: M.t): (M.m,M.m,M.m,M.m) =
    ( M.max (M.single x) M.zero
    , M.max (M.single x) M.zero
    , M.max (M.single x) M.zero
    , M.single x)

  let mss(xs: []M.t): M.m =
    let (x, _, _, _) =
      reduce redOp (M.zero,M.zero,M.zero,M.zero) (map mapOp xs) in
    x
}