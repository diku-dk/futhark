-- The 'frob' function should be compiled to sequential CPU code, and (very
-- importantly!) the memory arguments should be in DefaultSpace (that is, CPU
-- memory). Unfortunately we cannot write structure tests directly for this, so
-- instead we check that manifests are inserted to copy back and forth between
-- GPU memory.
-- ==
-- input { [1,2,3] }
-- output { [3,4,5] }
-- structure gpu-mem { GPUBody 0 Manifest 2 }

#[noinline] #[cpu_function]
def frob [n] (xs: *[n]i32) =
  map (+ 2) xs

entry main (xs: *[]i32) = frob xs
