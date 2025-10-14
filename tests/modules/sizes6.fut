local
module type sparse = {
  type^ mat = (nnz: i64) -> [nnz]i64
}

module sparse : sparse = {
  type^ mat = (nnz: i64) -> [nnz]i64
}
