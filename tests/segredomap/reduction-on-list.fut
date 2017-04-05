-- The reduction operator works on lists

let vec_add (xs : [#k]i32) (ys : [#k]i32) : [k]i32 =
  map (\x y -> x + y) xs ys

let main (xsss : [#l][#m][#n]i32): [l][n]i32 =
  let zeros = replicate n 0 in
  map (\(xss : [#m][#n]i32) : [n]i32  -> reduce vec_add zeros xss) xsss
