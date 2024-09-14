-- Simple test for fusing a map soac with a withacc
-- ==
-- entry: main

-- input { 3i64 4i64 [[12u32,11u32,10u32,9u32], [8u32,7u32,6u32,5u32], [4u32,3u32,2u32,1u32]] }
-- output { [0u32, 1u32, 10u32, 45u32, 26u32, 62u32, 180u32, 81u32, 160u32, 405u32, 166u32, 302u32] }

import "../../accs/intrinsics"

-- Q = E * T*T * (R*R+1)
def accUpdateO [Q] (T: i64) (R: i64) (k: i64) (Ok: [T*T][R*R+1]f32) (O: *[Q]f32) (A: [T*T]f32) : *[Q]f32 = #[unsafe]
  let inner = R*R+1
  let glb_offset = k * (T*T*inner)
  let f (Oacc: *acc ([Q]f32)) (tid,a) =
    let offset = glb_offset + tid*inner
    in
    loop Oacc for i < R do
      loop Oacc for j < R do
        let elm = Ok[tid][i*R+j] * a
        let ind = (offset + i*R) + j
        in  write Oacc ind elm
  in scatter_stream O f (zip (iota (T*T)) A)

def main (E: i64) (T: i64) (R: i64) (Ok: [E][T*T][R*R+1]f32) (O: *[E*(T*T)*(R*R+1)]f32) (A: [T*T]f32) : *[E*(T*T)*(R*R+1)]f32 = #[unsafe]
  let e = f32.i64 E
  let (_,O) =
  loop (A,O) for k < E-1 do
    let A' = map (\x -> (x+1.0+e)/(x+e-1.0) ) A
    let O = accUpdateO T R k Ok[k] O A'
    let A''= map (\x -> (x+2.0+e)/(x+e-2.0) ) A'
    let O = accUpdateO T R k Ok[k+1] O A''
    in  (A'',O)
  in O