-- This loop is interesting because only the final size is actually
-- used.  The simplifier can easily mess this up.  Contrived?  Yes,
-- but code generators sometimes do this.
--
-- ==
-- input { 0 [1] } output { 1i64 }
-- input { 1 [1] } output { 2i64 }
-- input { 2 [1] } output { 4i64 }
-- input { 3 [1] } output { 8i64 }

def main (n: i32) (as: []i32) : i64 =
  let as =
    loop (as) for _i < n do
      concat as as
  in length as
