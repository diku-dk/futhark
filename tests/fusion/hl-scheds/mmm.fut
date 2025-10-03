----------------------------------------------------------------------
-- High-Level Schedule for "traditional" dense matrix multiplication
-- that is not aimed at tensor cores, e.g., element type: f32 / f64
----------------------------------------------------------------------

def imap xs f = map f xs 

type real = f32

def pad1D 't [m] (xs: [m]t) (n : i64) (v: t) : [n]t =
  imap (iota n) (\i -> if i < m then xs[i] else v)
  
def strip2 (n: i64) = (n/64, 4i64, 16i64)
def strip1 (n: i64) = (n/16, 16i64)

def hlSched2D 't [m][n] ( xs : [m][n]t ) 
                        ( _offs: i64
                        , _lens: []i64
                        , _orig: []i64
                        , _perm: []i64
                        , _sigs: []i64
                        , _strs: []i64
                        ) : [m][n]t = xs

-- import "utils"

def padDotProd [M] (N: i64) (v: real) (xs: [M]real) (ys: [M]real) : real =
  map2 (*) (pad1D xs N v) (pad1D ys N v) |> reduce (+) 0

def mmm [M][N][Q] (A: [M][Q]real) (B: [Q][N]real) : [M][N]real =
  let (m, Tm, Rm) = strip2 M
  let (n, Tn, Rn) = strip2 N
  let (q, Tq)     = strip1 Q
  let C = imap A (\Arow -> imap (transpose B) (\Bcol -> padDotProd (q*Tq) 0 Arow Bcol )) 
  in  hlSched2D C ( Rm*Tn*Rn
                  , [m, Tm, Rm, n, Tn, Rn, q, Tq]  -- dims length
                  , [0,  0,  0, 1,  1,  1, 2,  2]  -- orig dimensions
                  , [0,  3,  6, 1,  4,  7, 2,  5]  -- dims permutation
                  , [0,  0,  2, 0,  0,  2, 2,  2]  -- signals, e.g., 3 means "seq+reg"
                  , [Tm*Rm*n*Tn*Rn, Rm*n*Tn*Rn, n*Tn*Rn, Tn*Rn, Rn, 1, 0, 0] -- strides
                  )

def main [M][N][Q] (A: [M][Q]real) (B: [Q][N]real) : [M][N]real =
  mmm A B
