-- | Maximum segment sums.

-- | Maximum segment sum with a custom measure.
let mss 't 'm (zero: m) (max: m -> m -> m) (combine: m -> m -> m)
              (single: t -> m)
              (ts: []t)
            : m =
  let redop (mssx, misx, mcsx, tsx) (mssy, misy, mcsy, tsy) =
        ( max mssx (max mssy (combine mcsx misy))
        , max misx (combine tsx misy)
        , max mcsy (combine mcsx tsy)
        , combine tsx tsy)
  let mapop x =
        ( max (single x) zero
        , max (single x) zero
        , max (single x) zero
        , single x)
  in (reduce redop (zero,zero,zero,zero) (map mapop ts)).1

-- | Maximum segment sum with an integer measure, which suffices for
-- most cases.
let mss' 't: (single: t -> i32) -> (ts: []t) -> i32 =
  mss 0 i32.max (+)
