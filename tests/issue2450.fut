-- Reproducer for wrong result from noncommutative segmented reduction on GPU backends.
-- ==
-- compiled random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output

def main xs = #[flattening(only_full)] map (reduce (\x y -> opaque x + y) 0i32) xs

-- Operands larger than maxTiledOperandWords (here 18 words), which take a
-- different code path.  Product of 3x3 matrices with wrapping arithmetic:
-- associative and exact, but not commutative.

type m3 = (i64, i64, i64, i64, i64, i64, i64, i64, i64)

def mm3 ((a0, a1, a2, a3, a4, a5, a6, a7, a8): m3)
        ((b0, b1, b2, b3, b4, b5, b6, b7, b8): m3) : m3 =
  ( a0 * b0 + a1 * b3 + a2 * b6
  , a0 * b1 + a1 * b4 + a2 * b7
  , a0 * b2 + a1 * b5 + a2 * b8
  , a3 * b0 + a4 * b3 + a5 * b6
  , a3 * b1 + a4 * b4 + a5 * b7
  , a3 * b2 + a4 * b5 + a5 * b8
  , a6 * b0 + a7 * b3 + a8 * b6
  , a6 * b1 + a7 * b4 + a8 * b7
  , a6 * b2 + a7 * b5 + a8 * b8
  )

def id3 : m3 = (1, 0, 0, 0, 1, 0, 0, 0, 1)

def row (r: [9]i64) : m3 = (r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8])

def unrow ((a0, a1, a2, a3, a4, a5, a6, a7, a8): m3) : [9]i64 =
  [a0, a1, a2, a3, a4, a5, a6, a7, a8]

-- ==
-- entry: wide_segmented
-- compiled random input { [100][512][9]i64 } auto output
-- compiled random input { [3][100000][9]i64 } auto output

entry wide_segmented [m] [n] (xsss: [m][n][9]i64) =
  #[flattening(only_full)] map (\xss -> unrow (reduce mm3 id3 (map row xss))) xsss

-- ==
-- entry: wide_nonsegmented
-- compiled random input { [100000][9]i64 } auto output
-- compiled random input { [1000003][9]i64 } auto output

entry wide_nonsegmented [n] (xss: [n][9]i64) =
  unrow (reduce mm3 id3 (map row xss))
