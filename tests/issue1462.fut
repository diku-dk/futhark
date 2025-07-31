def step [n] (buf: [n]f32) (r: i64): []f32 =
    let total = reduce (+) 0f32 buf
    in buf ++ [total * (f32.i64 r)]

def run [n] (t: i64) (T: i64) (buf: [n]f32) (r: i64): []f32 =
    (loop (buf) for i < t do step buf r) :> [T]f32

def runs (t: i64): [][]f32 =
    let start = [1f32, 1f32]
    in map (run t (t + 2) start) (iota 5)

-- ==
-- tags { no_opencl no_cuda no_cudatc no_hip no_pyopencl }
-- input { }
def main = runs 10
