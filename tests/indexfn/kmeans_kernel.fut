def infinity = i64.highest

def kmeansKer_body [num_cols] [nnz]
    (n: {i64 | (>= 0)})
    (row: {i64 | \row -> Range row (0,n)})
    (cluster: [num_cols]f32)
    (pointers: {[n + 1]i64 | \x -> Range x (0,infinity) && Monotonic (<=) x})
    (values: {[nnz]f32 | \_ -> nnz == pointers[n]})
    (indices_data: {[nnz]i64 | \x -> Range x (0, num_cols) && nnz == pointers[n]})
    : {f32 | \_ -> true} =
  let index_start = pointers[row]
  let nnz_sgm = pointers[row+1] - index_start
  in loop (correction) = (0) for j < nnz_sgm do
      let element_value = values[index_start+j]
      let column = indices_data[index_start+j]
      let cluster_value = cluster[column]
      let value = (element_value - 2 * cluster_value)*element_value
      in  correction+value

-- def kmeansKer [k] [num_cols] [nnz]
--     (n: {i64 | (>= 0)})
--     (pointers: {[n + 1]i64 | \x -> Range x (0,infinity) && Monotonic (<=) x})
--     (cluster_centers: [k][num_cols]f32)
--     (values: {[nnz]f32 | \_ -> nnz == pointers[n]})
--     (indices_data: {[nnz]i64 | \x -> Range x (0, num_cols)})
--     : {([n][k]f32) | \_ -> true} =
--   let diffs =
--     map (\row ->
--           map (\cluster ->
--                 kmeansKer_body n row cluster pointers values indices_data
--               ) cluster_centers
--         ) (iota n)
--   in diffs
