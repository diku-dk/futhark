-- A simple test for index-function generalization across a for loop
-- ==
-- input { [0, 1000, 42, 1001, 50000] }
-- output { 52043i32 }
-- structure gpu-mem { Manifest 0 }

def main [n] (a: [n]i32) : i32 =
  let b =
    loop xs = a[1:]
    for i < n / 2 - 2 do
      xs[i:]
  in reduce (+) 0 b
