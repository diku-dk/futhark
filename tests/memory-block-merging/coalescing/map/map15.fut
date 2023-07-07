-- Here we must be very careful not to short circuit the copy of
-- 'u_row' (which actually becomes a per-thread copy of 'u') with the
-- original consumed 'u'.  Each thread _does_ need a distinct copy of
-- 'u', and they cannot share memory.
-- ==
-- input { [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32], [7.0f32, 8.0f32, 9.0f32]] [42.0f32, 1337.0f32] }
-- output { [[[1.0f32, -40.0f32, 1683.0f32],
--           [4.0f32, -163.0f32, 6852.0f32],
--           [7.0f32, -286.0f32, 12021.0f32]],
--          [[1.0f32, -1335.0f32, 1784898.0f32],
--           [4.0f32, -5343.0f32, 7143597.0f32],
--           [7.0f32, -9351.0f32, 1.2502296e7f32]]] }
-- structure gpu-mem { Alloc 3 }

def tridagSeq [m] (y: *[m]f32) (beta: f32) =
    loop y
      for i < m-1 do
        let i    = i + 1
        let y[i] = y[i] - beta*y[i-1]
        in  y

def main [m][n] (u: *[n][m]f32) (betas: []f32) =
  map (\beta -> map (\u_row  -> tridagSeq (copy u_row) beta) u) betas
