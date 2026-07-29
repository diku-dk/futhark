-- Based on #2515.

def f [n] (A: *[n]f32) (A_bak: *[n]f32) : []f32 =
  let (A, _) =
    loop (A, A_bak) while length A > 0 do
      let m = length A - 1
      let A' = A_bak[:m]
      -- reuse the recycled buffer; size 'm' is variant
      in (A', A)
  -- 'A_bak' now holds the previous (variant-sized) 'A'
  in A
