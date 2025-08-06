-- ==
-- entry: rev_J
-- compiled random input { 0.5f32 0.7f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32} auto output

type real = f32
def real_sum = f32.sum

def dotprod xs ys = real_sum (map2 (*) xs ys)

def gemm [m] [n] [q] (alpha: real) (beta: real) (xss: [m][q]real, yss: [q][n]real, css: [m][n]real) =
  map2 (\xs cs -> map2 (\ys c -> c * alpha + beta * (dotprod xs ys)) (transpose yss) cs) xss css

entry rev_J [n] [m] [q] (alpha: real) (beta: real) (xss: [m][q]real) (yss: [q][n]real) (css: [m][n]real) (res_adj: [m][n]real) =
  vjp (gemm alpha beta) (xss, yss, css) res_adj
