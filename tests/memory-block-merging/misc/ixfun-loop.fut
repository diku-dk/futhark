-- A simple test for index-function generalization across a for loop
-- ==
-- input { [0, 1000, 42, 1001, 50000] }
-- output { 1249975000i32 }

let main [n] (a: [n]i32): i32 =
  let b = loop b = iota(10) for i < n do
          let m = a[i]
          in iota(m)
  in reduce (+) 0 b
