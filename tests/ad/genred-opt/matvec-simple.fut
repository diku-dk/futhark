-- ==
-- entry: rev_J
-- compiled random input { [1024][80]f32 [1024][80]f32 [1024][1024]f32} auto output
-- compiled random input { [1024][1024]f32 [1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input { [2048][1024]f32 [2048][1024]f32 [2048][2048]f32} auto output

-- input
-- {
-- [[1.0,2.0],[3.0,4.0]] [[5.0,6.0],[7.0,8.0]]
-- }

type real = f32
def real_sum = f32.sum

def dotprod xs ys = real_sum (map2 (*) xs ys)

def matvec [n] [q] (mat: [n][q]real) (vct: [q]real) : [n]real =
  map (dotprod vct) mat

def matmat [m] [n] [q] (mat1: [m][q]real, mat2: [n][q]real) : [m][n]real =
  map (matvec mat2) mat1

entry rev_J [m] [n] [q] (mat1: [m][q]real) (mat2: [n][q]real) (res_adj: [m][n]real) =
  vjp matmat (mat1, mat2) res_adj
