-- Test2 Memory-Block Merging
-- ==
-- input { [ [ [0, 1], [2, 3] ], [ [4, 5], [6, 7] ] ]  }
-- output { [[[0i32, 9i32], [0i32, 13i32]]]}
-- structure cpu { Alloc 1 }

let main(xsss: [#n][#n][#n]i32): [][n][n]i32 =
  let (_,asss) = split (1) xsss
  in  map (\ass ->
                map (\as ->
                        loop (r=0) = for i < n do
                            let r = r + as[i]
                            in  r
                        in
                        -- Will be merged.
                        loop (bs=iota(n)) = for j < n do
                            let bs[j] = bs[j]*r -- Will be merged.
                            in bs
                        in bs
                    )
                    ass
          ) asss
