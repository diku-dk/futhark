-- Simple test for vertical fusion of: map-withacc-map-withacc
-- ==
-- entry: main
--
-- input { 2i64 2i64 2i64
--         [ [ [12f32,11f32,10f32,9f32,11f32]
--           , [8f32, 7f32, 6f32, 5f32, 6f32]
--           , [4f32, 3f32, 2f32, 1f32, 4f32]
--           , [12f32, 7f32, 3f32,5f32,11f32]
--           ]
--         , [ [12f32, 7f32, 1f32,5f32,11f32]
--           , [ 5f32, 8f32, 7f32, 1f32,2f32]
--           , [12f32,11f32, 5f32,9f32,15f32]
--           , [6f32, 3f32, 7f32, 1f32, 4f32]
--           ]
--         ]
--         [ 120f32,110f32,100f32,90f32,110f32
--         , 80f32, 70f32, 60f32, 50f32, 60f32
--         , 40f32, 30f32, 20f32, 10f32, 40f32
--         , 120f32, 70f32, 30f32,50f32,110f32
--         , 120f32, 70f32, 10f32,50f32,110f32
--         ,  50f32, 80f32, 70f32, 10f32,20f32
--         , 120f32,110f32, 50f32,90f32,150f32
--         , 60f32, 30f32, 70f32, 10f32, 40f32
--         ]
--         [7f32, 13f32, 17f32, 11f32]
--       }
-- output { [ 50.399998f32, 29.399998f32, 4.2f32, 21.0f32, 110.0f32
--          , 22.5f32, 36.0f32, 31.5f32, 4.5f32, 60.0f32
--          , 55.199997f32, 50.6f32, 23.0f32, 41.399998f32, 40.0f32
--          , 26.57143f32, 13.285715f32, 31.000002f32, 4.4285717f32, 110.0f32
--          , 120.0f32, 70.0f32, 10.0f32, 50.0f32, 110.0f32
--          , 50.0f32, 80.0f32, 70.0f32, 10.0f32, 20.0f32
--          , 120.0f32, 110.0f32, 50.0f32, 90.0f32, 150.0f32
--          , 60.0f32, 30.0f32, 70.0f32, 10.0f32, 40.0f32
--          ]
--        }

-- input { 3i64 4i64 [[12u32,11u32,10u32,9u32], [8u32,7u32,6u32,5u32], [4u32,3u32,2u32,1u32]] }
-- output { [0u32, 1u32, 10u32, 45u32, 26u32, 62u32, 180u32, 81u32, 160u32, 405u32, 166u32, 302u32] }

import "../../accs/intrinsics"

-- Q = E * T*T * (R*R+1)
def accUpdateO [Q] (T: i64) (R: i64) (k: i64) (Ok: [T * T][R * R + 1]f32) (O: *[Q]f32) (A: [T * T]f32) : *[Q]f32 =
  let inner = R * R + 1
  let glb_offset = k * (T * T * inner)
  let f (Oacc: *acc ([Q]f32)) (tid, a) =
    let offset = glb_offset + tid * inner
    in loop Oacc for i < R do
         loop Oacc for j < R do
           let elm = Ok[tid][i * R + j] * a
           let ind = (offset + i * R) + j
           in write Oacc ind elm
  in scatter_stream O f (zip (iota (T * T)) A)

def main (E: i64) (T: i64) (R: i64) (Ok: [E][T * T][R * R + 1]f32) (O: *[E * (T * T) * (R * R + 1)]f32) (A: [T * T]f32) : *[E * (T * T) * (R * R + 1)]f32 =
  let e = f32.i64 E
  let (_, O) =
    loop (A, O) for k < E - 1 do
      let A' = map (\x -> (x + 1.0 + e) / (x + e - 1.0)) A
      let O = accUpdateO T R k Ok[k] O A'
      let A'' = map (\x -> (x + 2.0 + e) / (x + e - 2.0)) A'
      let O = accUpdateO T R k Ok[k + 1] O A''
      in (A'', O)
  in O
