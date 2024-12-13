-- ==
-- compiled nobench random input {[128][16][16]f16 [128][16][16]f16} error: CUDA call.*

def not_matmul (A: [16][16]f16) (B: [16][16]f16) : [16][16]f32 =
    map (\ Arow -> 
        map (\Brow -> 
            map2 (*) Arow Brow |> map f32.f16 |> reduce (+) 0.0f32)
        B
    ) A

entry main [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f32 =
  #[incremental_flattening(only_intra)]map2 not_matmul A B
