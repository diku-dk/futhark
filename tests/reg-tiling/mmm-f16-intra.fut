-- ==
-- compiled random input {[128][16][16]f16 [128][16][16]f16}


let dotproduct (x: [16]f16) (y: [16]f16) =
  map2 (*) x y
  |> map f32.f16
  |> reduce (+) 0

let matmul16 (A: [16][16]f16) (B: [16][16]f16) : [16][16]f32 =
    map (\ Arow -> 
        map (\Bcol -> 
            dotproduct Arow Bcol) 
        (transpose B)
    ) A     

entry main [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f32 =    
  #[incremental_flattening(only_intra)]map2 matmul16 A B


