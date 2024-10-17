-- entry: block_mmm
-- compiled input[2048i64, 16i64, 16i64]f16
--==
let dotproduct (x: [16]f16) (y: [16]f16) =
    map2 (*) x y |> 
    reduce (+) 0

let matmul16 (A: [16][16]f16) (B: [16][16]f16) : [16][16]f16 =
    map (\ Arow -> 
        map (\Bcol -> 
            dotproduct Arow Bcol) 
        (transpose B)
    ) A     

entry intra_block_mmm [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f16 =    
  #[incremental_flattening(only_intra)]map2 matmul16 A B

entry block_mmm [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f16 =
  map2 matmul16 A B
