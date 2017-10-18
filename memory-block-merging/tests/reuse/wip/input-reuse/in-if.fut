-- Would anyone need to do this?  Maybe not.  Does not work right now because we
-- do not look in sub-bodies of the loop body statements.  FIXME: While this
-- would work, what if the 'else' branch of the 'if' expression had another
-- index?  Then more analysis would be needed.
-- ==
-- input { [2, 5, 9] }
-- output { [3, 6, 0] }

-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let main [n] (xs: *[n]i32): [n]i32 =
  loop ys = replicate n 0 for i < n do
    let i' = i - 1
    let ys' = if i' >= 0
              then let ys[i'] = xs[i'] + 1
                   in ys
              else ys
    in ys'
