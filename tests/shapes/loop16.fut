-- Based on #2515.  The loop shrinks A by one each iteration until it is
-- empty, so main always returns an empty array; the point is that it type
-- checks and internalises.
-- ==
-- input { [1.0f32,2.0f32,3.0f32] [4.0f32,5.0f32,6.0f32] }
-- output { empty([0]f32) }

def f [n] (A: *[n]f32) (A_bak: *[n]f32) : []f32 =
  let (A, _) =
    loop (A, A_bak) while length A > 0 do
      let m = length A - 1
      let A' = A_bak[:m]
      -- reuse the recycled buffer; size 'm' is variant
      in (A', A)
  -- 'A_bak' now holds the previous (variant-sized) 'A'
  in A

entry main = f
