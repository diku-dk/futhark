-- ==
-- tags { no_wasm }
-- entry: big_to_small
-- no_wasm compiled input { [[1,2],[3,4]] [1,2] 3 }

-- ==
-- entry: small_to_big
-- no_wasm compiled input { [[1,2],[3,4]] [1,2] 3 }

def f 'a (x: a) (y: a) (z: a) = (x, y, z) 

entry big_to_small [n] (xss : [n][n]i32) (ys: [n]i32) (z: i32) : [n][n](i32,i32,i32) =
  f xss ys z

entry small_to_big [n] (xss : [n][n]i32) (ys: [n]i32) (z: i32) : [n][n](i32,i32,i32) =
  f z ys xss
