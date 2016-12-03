-- Test1 Memory-Block Merging
-- ==
-- input { [0, 3, 5, 7, 9] }
-- output { [6291264i32, 6291456i32, 6291584i32, 6291712i32, 6291840i32, 0i32, 6i32, 10i32, 14i32, 18i32]}

fun main(x: [n]int): []int =
  let y = map (*2) x in
  loop(a=y) = for i < n do
      let b = map (*2) a
      let c = map (+ (b[1])) b
      let d = map (+ (c[1])) c
      let e = map (+ (d[1])) d
      in  e
  in
  let w = concat a y
  in w

