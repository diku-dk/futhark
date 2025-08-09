-- Test2 Memory-Block Merging
--
-- For the CPU pipeline there is no coalescing to do in this program.  The
-- compiler makes sure there is only a single alloc before we even get to memory
-- block merging.
--
-- ==
-- input { [ [ [0i64, 1i64], [2i64, 3i64] ], [ [4i64, 5i64], [6i64, 7i64] ] ]  }
-- output { [[[0i64, 9i64], [0i64, 13i64]]]}
-- compiled random input { [128][128][128]i64 }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 3 }

def main [n] (xsss: [n][n][n]i64) : [][n][n]i64 =
  let asss = drop 1 xsss
  in map (\ass ->
            map (\as ->
                   let r =
                     loop r = 0
                     for i < n do
                       let r = r + as[i]
                       in r
                   in loop bs = iota n
                      for j < n do
                        let bs[j] = bs[j] * r
                        in bs)
                ass)
         asss
