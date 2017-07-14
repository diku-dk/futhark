-- Test2 Memory-Block Merging
--
-- There is no coalescing to do in this program.  The compiler makes sure there
-- is only a single alloc before we even get to memory block merging.
-- ==
-- input { [ [ [0, 1], [2, 3] ], [ [4, 5], [6, 7] ] ]  }
-- output { [[[0, 9], [0, 13]]]}
-- structure cpu { Alloc 1 }

let main (xsss: [#n][#n][#n]i32): [][n][n]i32 =
  let (_,asss) = split (1) xsss
  in  map (\ass ->
                map (\as ->
                        let r = loop r = 0 for i < n do
                            let r = r + as[i]
                            in  r
                        in
                        loop bs = iota n for j < n do
                            let bs[j] = bs[j]*r
                            in bs
                    )
                    ass
          ) asss
