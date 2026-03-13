def loop_body [num_cols] [nnz] [n]
    (row: {i64 | \row -> Range row (0,n)})
    (cluster: [num_cols]f32)
    (pointers: {[n + 1]i64 | \x -> Range x (0,nnz)})
    (values: {[nnz]f32 | \_ -> true})
    (indices_data: {[nnz]i64 | \x -> Range x (0,num_cols)})
    (correction: f32)
    (j: {i64 | \x -> Range x (0, pointers[row+1] - pointers[row])})
    : {f32 | \_ -> true} =
  let index_start = pointers[row]
  let element_value = values[index_start+j]
  let column = indices_data[index_start+j]
  let cluster_value = cluster[column]
  let value = (element_value - 2 * cluster_value)*element_value
  in  correction + value

def kmeansKer [num_cols] [nnz] [n]
    (row: {i64 | \row -> Range row (0,n)})
    (cluster: [num_cols]f32)
    (pointers: {[n + 1]i64 | \x -> Range x (0,nnz)})
    (values: {[nnz]f32 | \_ -> true})
    (indices_data: {[nnz]i64 | \x -> Range x (0,num_cols)})
    : {f32 | \_ -> true} =
  loop (correction) = (0) for j < pointers[row+1] - pointers[row] do
    loop_body row cluster pointers values indices_data correction j
