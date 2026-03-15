-- Manual version of sparse-kmeans for k=10
-- ==
-- compiled input @ data/movielens.in.gz
-- compiled input @ data/nytimes.in.gz
-- compiled input @ data/scrna.in.gz

-- compiled nobench input @ data/scrna.in.gz
-- output @ data/movielens.out

import "kmeans-sp-manual-static"


let main [nnz][np1] 
         (values: [nnz]f32)
         (indices_data: [nnz]i64) 
         (pointers: [np1]i64) =

  let fix_iter = false
  let threshold = 0.005f32
  let num_iterations = 10 --250i64
  let k = 10i64

  let (delta, i, cluster_centers) =
    kmeans_seq_rows fix_iter threshold num_iterations k
                    values indices_data pointers

  in  (cluster_centers[0,:33], delta, i)
