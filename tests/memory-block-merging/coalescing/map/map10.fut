-- ==
-- input { [0, 1, 2, 3, 4] }
-- output { [0f32, 1f32, 2f32, 3f32, 4f32] }
-- structure gpu-mem { Alloc 0 }
-- structure mc-mem { Alloc 0 }
-- structure seq-mem { Alloc 1 }

def main [n] (xs: *[n]i32) =
  map f32.i32 xs
