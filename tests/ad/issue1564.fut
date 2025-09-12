-- ==
-- tags { autodiff }
-- entry: kmeansSpAD

def costSparse [nnz] [np1] [cols] [k]
               (colidxs: [nnz]i64)
               (begrows: [np1]i64)
               (cluster_centers: [k][cols]f32) : f32 =
  let n = np1 - 1
  -- partial_distss : [n][k]f32
  let cluster = cluster_centers[0]
  let foo =
    let j = 0
    let correction = 0
    let (correction, _) =
      loop (correction, j) while j < 2 do
        let column = colidxs[j]
        let cluster_value = cluster[column]
        in (correction + cluster_value, j)
    in correction
  in foo

entry kmeansSpAD [nnz] [np1]
                 (k: i64)
                 (indices_data: [nnz]i64)
                 (pointers: [np1]i64) =
  jvp2 (\x -> vjp (costSparse indices_data pointers) x 1)
       (replicate k (replicate 1 1))
       (replicate k (replicate 1 1))
