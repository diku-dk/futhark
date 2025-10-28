-- ==
-- tags { no_python no_pyopencl no_c no_multicore no_ispc no_wasm no_wasm-multicore no_hip no_opencl no_cuda }
-- entry: test_no_matmul
-- compiled random input {[128][16][16]f16 [128][16][16]f16} error: CUDA call.*

-- ==
-- tags { no_python no_pyopencl no_c no_multicore no_ispc no_wasm no_wasm-multicore no_hip no_opencl no_cuda }
-- entry: test_wrong_types
-- compiled random input {[128][16][16]f32 [128][16][16]f32} error: CUDA call.*

def not_matmul (A: [16][16]f16) (B: [16][16]f16) : [16][16]f32 =
  map (\Arow ->
         map (\Brow ->
                map2 (*) Arow Brow |> map f32.f16 |> reduce (+) 0.0f32)
             B)
      A

def wrong_types (A: [16][16]f32) (B: [16][16]f32) : [16][16]f32 =
  map (\Arow ->
         map (\Brow ->
                map2 (*) Arow Brow |> reduce (+) 0.0f32)
             (transpose B))
      A

entry test_no_matmul [q] (A: [q][16][16]f16) (B: [q][16][16]f16) : [q][16][16]f32 =
  #[incremental_flattening(only_intra)] map2 not_matmul A B

entry test_wrong_types [q] (A: [q][16][16]f32) (B: [q][16][16]f32) : [q][16][16]f32 =
  #[incremental_flattening(only_intra)] map2 wrong_types A B
