def loop_body [num_cols] [nnz] [n]
    (row: i64)
    (cluster: [num_cols]f32)
    (pointers: [n + 1]i64)
    (values: [nnz]f32)
    (indices_data: [nnz]i64)
    (correction: f32)
    (j: i64)
    : f32 =
  let index_start = pointers[row]
  let element_value = values[index_start+j]
  let column = indices_data[index_start+j]
  let cluster_value = cluster[column]
  let value = (element_value - 2 * cluster_value)*element_value
  in  correction + value

def kmeansKer [num_cols] [nnz] [n]
    (row: i64)
    (cluster: [num_cols]f32)
    (pointers: [n + 1]i64)
    (values: [nnz]f32)
    (indices_data: [nnz]i64)
    : f32 =
  loop (correction) = (0) for j < pointers[row+1] - pointers[row] do
    loop_body row cluster pointers values indices_data correction j
