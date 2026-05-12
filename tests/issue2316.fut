-- ==
-- input { 2i64 }

module type blocked_square_regular = {
  type t
  type~ mat [n]
  val bsz : i64
  val mk [nz] : (n: i64) -> [nz](i64, i64, [bsz][bsz]t) -> mat [n]
  val unmk [n] : mat [n] -> ?[nz].[nz][bsz][bsz]t
}

module mat : blocked_square_regular with t = f64 = {
  type t = f64
  def bsz = 2i64

  type~ mat [n] =
    ?[nz].{ n: [n]()
          , blks: [nz][bsz][bsz]t
          }

  def mk [nz] (n: i64) (blks: [nz](i64, i64, [bsz][bsz]t)) : mat [n] =
    { n = replicate n ()
    , blks = map (.2) blks
    }

  def unmk [n] (x: mat [n]) = x.blks
}

entry main (n: i64) =
  let f i a = f64.sqrt (f64.i64 (i + 1)) + (f64.i64 (a + i) |> f64.sin |> (* 28.0))
  let diag_blks = map (\i -> (i, i, unflatten (map (f i) (iota (mat.bsz * mat.bsz))))) (iota n)
  in mat.unmk (copy (mat.mk (n * mat.bsz) diag_blks))
