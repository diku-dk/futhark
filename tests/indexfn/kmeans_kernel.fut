    -- with pre {
    --         np1 = n + 1,
    --         pointers.elem in [0,_],
    --         monotonic(pointers),
    --         nnz = pointers[n],
    --         indices_data.elem in [0, num_cols-1]
    --      },
    --      post { } =
type nat64 = {i64 | (>= 0)}

-- def loop_body [num_cols]
--     (n: i64)
--     (row: {i64 | \row -> 0 <= row && row < n})
--     (pointers: {[n + 1]nat64 | \pointers -> Monotonic (<=) pointers})
--     (nnz: {i64 | \nnz -> nnz == pointers[n]})
--     (cluster: [num_cols]f32)
--     (values: [nnz]f32)
--     (indices_data: [nnz]{i64 | \x -> 0 <= x && x < num_cols})
--     : {f32 | \_ -> true} =
--       let element_value = values[index_start+j]
--       -- ^ can prove: 0 <= index_start + j < nnz 
--       let column = indices_data[index_start+j] -- #[unsafe]
--       -- ^ can prove: 0 <= index_start + j < nnz
--       --   and infer: 0 <= columns <= num_cols-1
--       let cluster_value = cluster[column]
--       -- ^ can prove index within bounds.
--       let value = (element_value - 2 * cluster_value)*element_value
--       in  correction+value
let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def map_body [num_cols]
    (n: i64)
    (row: {i64 | \row -> 0 <= row && row < n})
    (pointers: {[n + 1]nat64 | \pointers -> Monotonic (<=) pointers})
    (nnz: {i64 | \nnz -> nnz == pointers[n]})
    (cluster: [num_cols]f32)
    (values: [nnz]f32)
    (indices_data: [nnz]{i64 | \x -> 0 <= x && x < num_cols})
    : {f32 | \_ -> true} =
  let index_start = pointers[row]
  let nnz_sgm = pointers[row+1] - index_start
  -- ^ can deduce nnz_sgm >= 0, but not needed
  let corrections = map (\j ->
      let element_value = values[index_start+j]
      -- ^ can prove: 0 <= index_start + j < nnz 
      let column = indices_data[index_start+j] -- #[unsafe]
      -- ^ can prove: 0 <= index_start + j < nnz
      --   and infer: 0 <= columns <= num_cols-1
      let cluster_value = cluster[column]
      -- ^ can prove index within bounds.
      let value = (element_value - 2 * cluster_value)*element_value
      in value
    ) (iota nnz_sgm)
  let correction = if nnz_sgm > 0 then (scan (+) 0 corrections)[nnz_sgm-1] else 0
  in correction
  -- in loop (correction) = (0) for j < nnz_sgm do
  --     let element_value = values[index_start+j]
  --     -- ^ can prove: 0 <= index_start + j < nnz 
  --     let column = indices_data[index_start+j] -- #[unsafe]
  --     -- ^ can prove: 0 <= index_start + j < nnz
  --     --   and infer: 0 <= columns <= num_cols-1
  --     let cluster_value = cluster[column]
  --     -- ^ can prove index within bounds.
  --     let value = (element_value - 2 * cluster_value)*element_value
  --     in  correction+value

-- def kmeansKer [k] [num_cols]
--     (n: i64)
--     (pointers: {[n + 1]nat64 | \pointers -> Monotonic (<=) pointers})
--     (nnz: {i64 | \nnz -> nnz == pointers[n]})
--     (cluster_centers: [k][num_cols]f32)
--     (values: [nnz]f32)
--     (indices_data: [nnz]{i64 | \x -> 0 <= x && x < num_cols})
--     : {([n][k]f32) | \_ -> true} =
--   let diffs =
--     map (\row -> 
--           map (\cluster ->     
--                 let index_start = pointers[row]
--                 let nnz_sgm = pointers[row+1] - index_start
--                 -- ^ can deduce nnz_sgm >= 0, but not needed
--                 in loop (correction) = (0) for j < nnz_sgm do
--                     let element_value = values[index_start+j]
--                     -- ^ can prove: 0 <= index_start + j < nnz 
--                     let column = indices_data[index_start+j] -- #[unsafe]
--                     -- ^ can prove: 0 <= index_start + j < nnz
--                     --   and infer: 0 <= columns <= num_cols-1
--                     let cluster_value = cluster[column]
--                     -- ^ can prove index within bounds.
--                     let value = (element_value - 2 * cluster_value)*element_value
--                     in  correction+value
--               ) cluster_centers
--         ) (iota n)
--   in diffs

