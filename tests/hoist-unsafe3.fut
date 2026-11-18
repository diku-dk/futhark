-- An unsafe array-producing statement (here a reshape) hoisted out of a map
-- inside a kernel is protected by a fallback branch. The allocation in the
-- fallback branch must still be hoisted out of the kernel.
-- ==
-- input { [[1f32,2f32,3f32,4f32],[5f32,6f32,7f32,8f32]] 2i64 2i64 3i64 }
-- output { [[4f32,6f32,4f32],[12f32,14f32,12f32]] }
-- input { [[1f32,2f32,3f32,4f32],[5f32,6f32,7f32,8f32]] 3i64 1i64 0i64 }
-- output { [empty([0]f32),empty([0]f32)] }
-- structure gpu-mem { SegMap/Alloc 0 }

def main [n] [m] (xss: [n][m]f32) (k: i64) (l: i64) (a: i64) =
  map (\xs ->
         map (\i -> f32.sum (map (\r -> r[i % l]) (unflatten (xs :> [k * l]f32))))
             (iota a))
      xss
