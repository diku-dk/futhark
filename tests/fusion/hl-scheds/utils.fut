-- Generics
def imap  xs f = map f xs
def imap2 xs ys f = map2 f xs ys
type real = f32

-------------------------------------------------------------
-- Special functions for padding and "tiling" a dimension ---
-------------------------------------------------------------

def pad1D  [m] 't (X: [m]t) (n: i64) (v: t) : [n]t =
  imap (iota n) (\i -> if i < m then #[unsafe] X[i] else v)

def strip1 (M: i64) : (i64, i64)      = ( (M + 15)/16, 16 )
def strip2 (M: i64) : (i64, i64, i64) = ( (M + 63)/64, 16, 4 )

---------------------------------------------------------------------
-- Special function for defining a high-level schedule as an LMAD ---
---------------------------------------------------------------------

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

