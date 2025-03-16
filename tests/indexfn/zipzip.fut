def zipzip [n] (edges: [n](i64,i64)) (ids: [n]i64) : {[n]((i64,i64), (i64,i64)) | \_ -> true} =
  let ids2 = map (\i -> (2*i, 2*i+1)) ids
  in zip edges ids2

-- def zipzip [n] (xs: [n]i64) (ys: [n]i64) =
--   zip xs ys

-- def main n : {[n]((i64,i64), (i64,i64)) | \_ -> true} =
--   let is = map (\i -> (2*i, 2*i+1)) (iota n)
--   in zipzip is is
