-- Indexing an array literal with a constant should remove the
-- indexing.
--
-- ==
-- structure { Index 0 Assert 0 }

let main(xs: []i32): []i32 =
  let xss = [xs]
  in xss[0]
