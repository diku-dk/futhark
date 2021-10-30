-- ==
-- entry: rev_J
-- compiled random input { [1024][80]f32 [80][1024]f32 [1024][1024]f32} auto output
-- compiled random input { [1024][1024]f32 [1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input { [2048][1024]f32 [1024][2048]f32 [2048][2048]f32} auto output

-- compiled input
-- {
-- [[1.0,2.0],[3.0,4.0]] [[5.0,6.0],[7.0,8.0]]
-- }

type real = f32
let real_sum = f32.sum

let dotprod xs ys = real_sum (map2 (*) xs ys)

let matmul [m][n][q] (xss: [m][q]real, yss: [q][n]real) =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

entry rev_J [n][m][q] (xss: [m][q]real) (yss: [q][n]real) (res_adj: [m][n]real) =
  vjp matmul (xss, yss) res_adj

