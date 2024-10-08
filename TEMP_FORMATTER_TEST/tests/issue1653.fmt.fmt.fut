module type mat =
{
  type t
  type~ mat [n] [m]
  val eye: (n: i64) -> (m: i64) -> mat [n] [m]
  val dense [n] [m]: mat [n] [m] -> [n][m]t
}

module type sparse =
{
  type t
  type~ csr [n] [m]
  type~ csc [n] [m]
  module csr: {
    include matwith t  = twith mat [n] [m] = csr [n] [m]
  }
  module csc: {
    include matwith t  = twith mat [n] [m] = csc [n] [m]
  }
}

module sparse (T: numeric): sparsewith t  = T.t = {
  type t = T.t
  
  module csr = {
    type t = t
    
    type~ mat[n] [m] = ?[nnz].{dummy_m: [m]()
    ,row_off: [n]i64
    ,col_idx: [nnz]i64
    ,vals: [nnz]t}
    
    def eye (n: i64) (m: i64): mat [n] [m] =
      let e = i64.min n m
      let one = T.i64 1
      let row_off = (map (+ 1) (iota e) ++ replicate (i64.max 0 (n - e)) e) :> [n]i64
      in
        {dummy_m = replicate m ()
        ,row_off = row_off
        ,col_idx = iota e
        ,vals = replicate e one}
    
    def dense [n] [m] (_csr: mat [n] [m]): [n][m]t =
      let arr: *[n][m]t = tabulate_2d n m (\_ _ -> T.i64 0)
      in arr
  }
  
  module csc = {
    type t = t
    
    def eye (n: i64) (m: i64): csr.mat [m] [n] =
      csr.eye m n
    
    def dense [n] [m] (mat: csr.mat [n] [m]): [m][n]t =
      csr.dense mat |> transpose
    
    type~ mat [n] [m] = csr.mat [m] [n]
  }
  
  type~ csr [n] [m] = csr.mat [n] [m]
  
  type~ csc [n] [m] = csc.mat [n] [m]
}

module spa = sparse i32

module csr = spa.csr

def main (n: i64) (m: i64): *[n][m]i32 =
  csr.eye n m |> csr.dense