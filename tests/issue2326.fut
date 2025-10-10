module type blocked_square_regular = {
  type~ mat [n]

  val from_coo : (n: i64) -> mat [n]
}

module mat : blocked_square_regular = {
  def bsz = 8i64
  def zero_t = f64.i64 0
  def one_t = f64.i64 1

  def idx_unflatten (n: i64) (i: i64) : (i64, i64) =
    (i / n, i % n)

  def idx_flatten (n: i64) (r: i64, c: i64) : i64 =
    r * n + c

  type~ mat [n] =
    ?[nz].{ n: [n]()
          , idxs: [nz]i64
          , blks: [nz][bsz][bsz]f64
          }

  def from_coo (n: i64) : mat [n] =
    let nz = 12i64
    in { n = replicate n ()
       , idxs = iota nz
       , blks = tabulate_3d nz bsz bsz (\i j l -> f64.i64 (i + j + l))
       }
}

type coo = (i64, i64, f64)

entry rlu = mat.from_coo 128

entry blks_rlu = rlu
