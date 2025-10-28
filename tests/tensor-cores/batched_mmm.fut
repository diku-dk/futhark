-- ==
-- tags { no_cuda no_hip no_opencl no_pyopencl }
-- entry: mmm_intra16
-- compiled random input {[128][16][16]f16 [128][16][16]f16} auto output

-- ==
-- entry: mmm_intra32
-- compiled random input {[128][32][32]f16 [128][32][32]f16} auto output

-- ==
-- entry: mmm_intra64
-- compiled random input {[128][64][64]f16 [128][64][64]f16} auto output
import "mmm-intra-helpers"

def mmm_intra [q] [d] (A: [q][d][d]f16) (B: [q][d][d]f16) : [q][d][d]f32 =
  #[incremental_flattening(only_intra)] map2 matmul_f16_f32 A B

entry mmm_intra16 [q] (A: [q][16][16]f16) (B: [q][16][16]f16) =
  mmm_intra A B

entry mmm_intra32 [q] (A: [q][32][32]f16) (B: [q][32][32]f16) =
  mmm_intra A B

entry mmm_intra64 [q] (A: [q][64][64]f16) (B: [q][64][64]f16) =
  mmm_intra A B
