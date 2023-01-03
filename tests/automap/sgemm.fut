-- ==
-- entry: main
-- random input { [5][10]f32 [10][3]f32 [5][3]f32 f32 f32 }
-- output { true }

def mult_orig [n][m][p] (xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  let dotprod xs ys = f32.sum (map2 (*) xs ys)
  in map (\xs -> map (dotprod xs) (transpose yss)) xss

def add [n][m] (xss: [n][m]f32, yss: [n][m]f32): [n][m]f32 =
  map2 (map2 (+)) xss yss

def scale [n][m] (xss: [n][m]f32, a: f32): [n][m]f32 =
  map (map1 (*a)) xss

def main_orig [n][m][p] (ass: [n][m]f32) (bss: [m][p]f32) (css: [n][p]f32)
                   (alpha: f32) (beta: f32)
                 : [n][p]f32 =
  add(scale(css,beta), scale(mult_orig(ass,bss), alpha))

  
def mult_am [n][m][p] (xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  f32.sum ((transpose (replicate p xss)) * (replicate n (transpose yss)))

def main_am [n][m][p] (ass: [n][m]f32) (bss: [m][p]f32) (css: [n][p]f32)
                   (alpha: f32) (beta: f32)
                 : [n][p]f32 =
  css*beta + mult_am(ass,bss)*alpha
                           
entry main [n][m][p] (ass: [n][m]f32) (bss: [m][p]f32) (css: [n][p]f32)
                   (alpha: f32) (beta: f32) =
  main_orig ass bss css alpha beta == main_am ass bss css alpha beta
