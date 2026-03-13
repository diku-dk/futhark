def kmeansKer [num_cols] [nnz] [n]
    (row: {i64 | \row -> Range row (0,n)})
    (cluster: [num_cols]f32)
    (pointers: {[n + 1]i64 | \x -> Range x (0,nnz)})
    (values: {[nnz]f32 | \_ -> true})
    (indices_data: {[nnz]i64 | \x -> Range x (0,num_cols)})
    : {f32 | \_ -> true} =
  let index_start = pointers[row]
  let nnz_sgm = pointers[row+1] - index_start
  -- There's no need to extract this loop as a top-level
  -- function because we don't annotate any properties on it.
  in loop (correction) = (0) for j < nnz_sgm do
      let element_value = values[index_start+j]
      let column = indices_data[index_start+j]
      let cluster_value = cluster[column]
      let value = (element_value - 2 * cluster_value)*element_value
      in  correction+value
