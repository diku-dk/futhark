-- Simple test for fusing a map soac within a following withacc
-- ==
-- entry: main
--
-- input { 3i64 4i64 
--         [[12f32,11f32,10f32,9f32], [8f32,7f32,6f32,5f32], [4f32,3f32,2f32,1f32]]
--         [7f32, 13f32, 17f32]
--         [120f32,110f32,100f32,90f32,80f32,70f32,60f32,50f32,40f32,30f32,20f32,111f32]
--       }
-- output { [168.0f32, 208.0f32, 136.0f32, 154.0f32, 182.0f32, 102.0f32, 140.0f32, 156.0f32, 68.0f32, 126.0f32, 130.0f32, 34.0f32] }


import "../../accs/intrinsics"

def update (T:i64) (R:i64) (Oacc: *acc ([T*R]f32)) (tid: i64, a: f32, Qk: [R]f32) : *acc ([T*R]f32) =
  loop Oacc for i < R do
    let elm = Qk[i] * a
    let ind = i*T + tid
    in  write Oacc ind elm

entry main (T: i64) (R: i64) (Q: [T][R]f32) (A: [T]f32) (O: *[T*R]f32) : *[T*R]f32 =
  let A' = map (*2.0) A in
  let z3 = zip3 (iota T) A' Q
  in  scatter_stream O (update T R) z3


