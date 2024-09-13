-- Simple test for fusing scatter-flatten with the preceding 
-- map nest that produces its indices and values
-- ==
-- entry: main
-- input { [[12i32,11i32,10i32,9i32], [8i32,7i32,6i32,5i32], [4i32,3i32,2i32,1i32]] }
--
-- output { [12.0f32, 14.0f32, 18.0f32, 24.0f32, 32.0f32, 42.0f32, 54.0f32, 68.0f32, 84.0f32, 102.0f32, 122.0f32, 144.0f32] }

entry main [m] [b] (ass : [m][b]i32) =
  let indvals = map (map2 (\ i a -> (i64.i32 a + i, f32.i32 (a*a))) (iota b) ) ass
  let (finds, fvals) = flatten indvals |> unzip
  let tmp = replicate (m*b) 0
  let finds' = map2 (\ ind i -> ind - (1 + i % b) ) finds (iota (m*b))
  let fvals' = map2 (\ vla i -> vla + f32.i64 i ) fvals (iota (m*b))
  let res = scatter tmp finds' fvals'
  in  res 
  
