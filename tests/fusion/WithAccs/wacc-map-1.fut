-- Simple test for fusing a map soac within a following withacc
-- ==
-- entry: main
--
-- input { 3i64 4i64 
--         [[12f32,11f32,10f32,9f32], [8f32,7f32,6f32,5f32], [4f32,3f32,2f32,1f32]]
--         [7f32, 13f32, 17f32]
--         [120f32,110f32,100f32,90f32,80f32,70f32,60f32,50f32,40f32,30f32,20f32,111f32]
--       }
-- output { [108.0f32, 120.0f32, 76.0f32, 99.0f32, 105.0f32, 57.0f32, 90.0f32, 90.0f32, 38.0f32, 81.0f32, 75.0f32, 19.0f32]
--          [11.0f32, 17.0f32, 21.0f32]
--        }

import "../../accs/intrinsics"

def update (T:i64) (R:i64) (Oacc: *acc ([T*R]f32)) (tid: i64, a: f32, Qk: [R]f32) : *acc ([T*R]f32) =
  loop Oacc for i < R do
    let elm = Qk[i] * a
    let ind = i*T + tid
    in  write Oacc ind elm

entry main (T: i64) (R: i64) (Q: [T][R]f32) (A: [T]f32) (O: *[T*R]f32) : (*[T*R]f32, *[T]f32) =
  let A' = map (+2.0) A
  let z3 = zip3 (iota T) A' Q
  let r' = scatter_stream O (update T R) z3
  let A''= map (+2.0) A'
  in  (r', A'')


