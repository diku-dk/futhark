-- ==
-- structure distributed { SegStencil 1 }

entry main arr =
  let invariant = map (map (const 1i32)) (copy arr)
  let lambda c v = c + v[0] + v[1]
  let ixs = [(-1,0),(1,0)]
  in stencil_2d ixs lambda invariant arr
