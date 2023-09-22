-- Removal of invariant of invariant loop parameter (and eventually entire loop).
-- ==
-- structure { DoLoop 0 }

entry main [n] (bs: [n]bool) =
  let res =
    loop (x, y) = (0i32, false)
      for i < n do
        let y' = bs[i] && y
        let x' = x + (i32.bool y')
        in  (x', y')
   in res
