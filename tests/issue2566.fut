-- Simplifying an index into an array literal or an update must not
-- forward to an array that is consumed before or after the index, or
-- if the index result is itself consumed.

-- ==
-- entry: lit_consume_result
-- input { [1,2,3] [4,5,6] } output { [9,5,6] [4,5,6] }

entry lit_consume_result (ys: [3]i32) (zs: *[3]i32) =
  let m = [zs, ys]
  let r = m[0]
  let r[0] = 9
  in (r, zs)

-- ==
-- entry: lit_consume_after
-- input { [1,2,3] [4,5,6] } output { [4,5,6] [9,5,6] }

entry lit_consume_after (ys: [3]i32) (zs: *[3]i32) =
  let m = [zs, ys]
  let r = m[0]
  let zs[0] = 9
  in (r, zs)

-- ==
-- entry: lit_consume_before
-- input { [1,2,3] [4,5,6] } output { [4,5,6] [9,5,6] }

entry lit_consume_before (ys: [3]i32) (zs: *[3]i32) =
  let m = [zs, ys]
  let zs[0] = 9
  let r = m[0]
  in (r, zs)

-- ==
-- entry: update_consume_result
-- input { [[1,2,3],[1,2,3]] [4,5,6] } output { [9,5,6] [4,5,6] }

entry update_consume_result (xs: *[2][3]i32) (zs: [3]i32) =
  let xs[0] = zs
  let r = xs[0]
  let r[0] = 9
  in (r, zs)

-- ==
-- entry: update_consume_after
-- input { [[1,2,3],[1,2,3]] [4,5,6] } output { [4,5,6] [9,5,6] }

entry update_consume_after (xs: *[2][3]i32) (zs: *[3]i32) =
  let xs[0] = zs
  let r = xs[0]
  let zs[0] = 9
  in (r, zs)

-- ==
-- entry: update_consume_before
-- input { [[1,2,3],[1,2,3]] [4,5,6] } output { [4,5,6] [9,5,6] }

entry update_consume_before (xs: *[2][3]i32) (zs: *[3]i32) =
  let xs[0] = zs
  let zs[0] = 9
  let r = xs[0]
  in (r, zs)
