// Test a simple map, that might be irregular based on input data.
fun [[int]] main([int] a) =
  map(fn [int] (int n) => iota(n), a)
