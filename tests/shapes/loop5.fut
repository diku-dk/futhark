-- More complex loop.  We must infer that the size here is unchanging.
-- ==
-- input { [1,2,3] } output { [1,2,3] }

let main [n] (s: [n]i32) : [n]i32 =
  loop s for _i < 10 do tabulate n (\i -> s[i])
