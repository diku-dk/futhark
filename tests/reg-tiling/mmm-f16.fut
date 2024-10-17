-- ==
-- compiled random input {[65536][16][16]f16 [65536][16][16]f16}


let dotproduct (x: [16]f16) (y: [16]f16) =
    map2 (*) x y |> 
    reduce (+) 0

let matmul16 (A: [16][16]f16) (B: [16][16]f16) : [16][16]f16 =
    map (\ Arow -> 
        map (\Bcol -> 
            dotproduct Arow Bcol) 
        (transpose B)
    ) A     

entry main [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f16 =    
  map2 matmul16 A B

