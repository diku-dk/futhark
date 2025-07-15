----------------------------------------------------------------------
-- High-Level Schedule for "traditional" dense matrix multiplication
--  that is not aimed at tensor cores, e.g., element type: f32 / f64
-- The reduce o map is written as a loop
----------------------------------------------------------------------

import "utils"

def padDotProd [M] (N: i64) (v: real) (xs: [M]real) (ys: [M]real) : real =
  let xs' = pad1D xs N v
  let ys' = pad1D ys N v
  in  loop s = 0 for i < N do
        s + xs'[i] * ys'[i]

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
