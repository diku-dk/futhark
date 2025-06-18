----------------------------------------------------------------------
-- High-Level Schedule for "traditional" dense matrix multiplication
-- that is not aimed at tensor cores, e.g., element type: f32 / f64
----------------------------------------------------------------------

-- Generics
def imap  xs f = map f xs
def imap2 xs ys f = map2 f xs ys
type real = f32

-- Special functions for padding and "tiling" a dimension
def pad1D  [m] 't (X: [m]t) (n: i64) (v: t) : [n]t =
  imap (iota n) (\i -> if i < m then #[unsafe] X[i] else v)

def strip1 (M: i64) : (i64, i64)      = ( (M + 15)/16, 16 )
def strip2 (M: i64) : (i64, i64, i64) = ( (M + 63)/64, 16, 4 )

-- Special function for defining a high-level schedule as a LMAD
def hlSched2D [m][n][q] 't 
      (X: [m][n]t) 
      ( _offs: i64
      , _lens: [q]i64
      , _orig: [q]i64
      , _sigma: [q]i64
      , _signals: [q]i64
      , _strides: [q]i64
      ) : [m][n]t
 = X
 
------------------------------
-- Actual Code Starts Here: --
------------------------------

def padDotProd [M] (N: i64) (v: real) (xs: [M]real) (ys: [M]real) : real =
  map2 (*) (pad1D xs N v) (pad1D ys N v) |> reduce (+) 0

def mmm [M][N][Q] (A: [M][Q]real) (B: [Q][N]real) : [M][N]real =
  let (m, Tm, Rm) = strip2 M
  let (n, Tn, Rn) = strip2 N
  let (q, Tq)     = strip1 Q
  let C = imap A (\Arow -> imap (transpose B) (\Bcol -> padDotProd (q*Tq) 0 Arow Bcol )) 
  in  hlSched2D C ( 0
                  , [m, Tm, Rm, n, Tn, Rn, q, Tq]  -- dims length
                  , [0,  0,  0, 1,  1,  1, 2,  2]  -- orig dimensions
                  , [0,  3,  6, 1,  4,  7, 2,  5]  -- dims permutation
                  , [0,  0,  3, 0,  0,  3, 0,  0]  -- signals, e.g., 3 means "seq+reg"
                  , [Tm*Rm*n*Tn*Rn, Rm*n*Tn*Rn, n*Tn*Rn, Tn*Rn, Rn, 1, 0, 0] -- strides
                  )

def main [M][N][Q] (A: [M][Q]real) (B: [Q][N]real) : [M][N]real =
  mmm A B
