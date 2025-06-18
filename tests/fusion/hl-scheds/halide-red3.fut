-- Halide's Extreme Case 1: fully parallel + 3x redundant computation
-- inp:[M+2][N+2]int;   blur: [M+2][N]int;   out: [M][N]int

#[noinline]
def fuseSched2D [m][n] (arr: [m][n]f32) (_offs: i64, _dims: [](i64,i64,i64)) : [m][n]f32 =
  arr 

def main1 (M: i64) (N: i64) (inp: [M+2][N+2]f32) : [M][N]f32 = #[unsafe]
  let blur_o = tabulate_2d (M+2) N (\y x -> inp[y,x] + inp[y,x+1] + inp[y,x+2])
  let blur   = fuseSched2D blur_o (0, [(M,N,0),(N,1,0),(3,N,3)])
  let out    = tabulate_2d M N (\y x -> blur[y,x] + blur[y+1,x] + blur[y+2,x]) 
  in  out


def main [M][N] (inp: [M][N]f32) : [M-2][N-2]f32 = #[unsafe]
  let blur_o = tabulate_2d M (N-2) (\y x -> inp[y,x] + inp[y,x+1] + inp[y,x+2])
  let blur   = fuseSched2D blur_o (0, [(M-2,N-2,0),(N-2,1,0),(3,N-2,3)])
  let out    = tabulate_2d (M-2) (N-2) (\y x -> blur[y,x] + blur[y+1,x] + blur[y+2,x])
  in  out

