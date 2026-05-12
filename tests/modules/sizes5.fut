local
module type sparse = {
  type~ mat = ?[nnz].[nnz]i64
}

module sparse : sparse = {
  type~ mat = ?[nnz].[nnz]i64
}
