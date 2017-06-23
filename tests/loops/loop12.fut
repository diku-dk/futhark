-- This loop is interesting because only the final size is actually
-- used.  The simplifier can easily mess this up.  Contrived?  Yes,
-- but code generators sometimes do this.
--
-- ==
-- input { 0 [1] } output { 1 }
-- input { 1 [1] } output { 2 }
-- input { 2 [1] } output { 4 }
-- input { 3 [1] } output { 8 }

let main (n: i32) (as: []i32): i32 =
  let as = loop (as) for _i < n do
    concat as as
  in (shape as)[0]
