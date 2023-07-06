-- Here we must be very careful not to short circuit the copy of
-- 'u_row' (which actually becomes a per-thread copy of 'u') with the
-- original consumed 'u'.  Each thread _does_ need a distinct copy of
-- 'u', and they cannot share memory.
-- ==
-- structure gpu-mem { Alloc 3 }

def tridagSeq [m] (y: *[m]f32) (beta: f32) =
    loop y
      for i < m do
        let i    = i + 1
        let y[i] = y[i] - beta*y[i-1]
        in  y

def main [m][n] (u: *[n][m]f32) (betas: []f32) =
  map (\beta -> map (\u_row  -> tridagSeq (copy u_row) beta) u) betas
