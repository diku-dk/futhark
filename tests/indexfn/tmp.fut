-- ~~loopbody_t [d₇]i64
-- ~~mergepat_t [loop_d₀]i64
-- ~~pat_t [loop_d₀]i64
-- ~~loopbody_t' [d₇]i64
-- ~~merge_t' [loop_d₀]i64
-- ~~sparams [VName (Name "loop_d\8320") 4603]
-- ~~init_substs []
-- ~~mergepat''((ys: [loop_d₀]i64))
-- def f [n] (ys: [n]i64) (p: i64 -> bool) =
-- 
def f [n] (ys: {[n]i64 | \y -> Monotonic (<) y}) (p: i64 -> bool) =
  loop (ys) while (length ys > 0) do
    filter p ys

def main ys =
  f ys (\_ -> false)
