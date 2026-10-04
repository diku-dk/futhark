-- By parametricity, the results of "transpose" and "reverse" alias only
-- their arguments, so they can be consumed when the arguments can.
-- ==
-- input { [[1,2],[3,4]] [5,6,7] }
-- output { [[0,3],[2,4]] [0,6,5] }

entry main (xss: *[2][2]i32) (xs: *[3]i32) =
  let yss = transpose xss
  let yss[0, 0] = 0
  let ys = xs |> reverse
  let ys[0] = 0
  in (yss, ys)
