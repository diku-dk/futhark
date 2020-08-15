-- 1024 8192 65536 524288 4194304 33554432 
-- ==
-- entry: plusint
-- random input { [1073741824]i32 } auto output

-- input @kA-1024.data auto output
-- input @kA-8192.data auto output
-- input @kA-65536.data auto output
-- input @kA-524288.data auto output
-- input @kA-4194304.data auto output
-- input @kA-33554432.data auto output
-- random input { [268435456]i32 } auto output
entry plusint input =
  scan (+) 0 input

-- 
-- entry: mulint
-- input @kA-1024.data auto output
-- input @kA-8192.data auto output
-- input @kA-65536.data auto output
-- input @kA-524288.data auto output
-- input @kA-4194304.data auto output
-- input @kA-33554432.data auto output
-- random input { [268435456]i32 } auto output
entry mulint input =
  scan (*) 0 input

-- 
-- entry: plusfloat
-- input @kA-1024f.data auto output
-- input @kA-8192f.data auto output
-- input @kA-65536f.data auto output
-- input @kA-524288f.data auto output
-- input @kA-4194304f.data auto output
-- input @kA-33554432f.data auto output
-- random input { [268435456]f32 } auto output
entry plusfloat (input:[]f32) =
  scan (+) 0.0 input

--  
-- entry: plustuples
-- input @kA-1024f.data auto output
-- input @kA-8192f.data auto output
-- input @kA-65536f.data auto output
-- input @kA-524288f.data auto output
-- input @kA-4194304f.data auto output
-- input @kA-33554432f.data auto output
-- random input { [268435456]f32 } auto output
entry plustuples (input:[]f32) : []f32 =
    let (a,b) = unzip <| scan (\(a1,a2) (b1,b2) -> ((a1+b1), (a2+b2))) (0.0,0.0) <| zip input input in a
