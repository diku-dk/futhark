-- Test1 Memory-Block Merging
-- ==
-- input { [0, 3, 5, 7, 9, 11] }
-- output { [0i32, 384i32, 640i32, 896i32, 1152i32, 1408i32, 14i32, 18i32, 22i32] }

fun main(x: [n]i32): []i32 =
  let y = map (*2) x in
  let y'= reshape (2,n/2) y
  loop(a=y) = for i < n do
      let b = map (*2) a
      let c = map (+ (b[0])) b
      let d = map (+ (c[0])) c
      let e = map (+ (d[0])) d
      in  e
  in
  let w = concat a (y'[1])
  in w

