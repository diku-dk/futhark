-- Example program from the ARRAY'14 paper.
-- ==
-- tags { no_opencl }

fun [[f64]] main(*[int] X, *[[f64]] A) =
  let M = size(0,A) in
  let N = size(1,A) in
  map (fn *[f64] ((int, *[f64]) e) =>
         let (i, a) = e in
         loop(a) = for j < N do
           let a[j] = a[ X[j] ] * 2.0 in a
         in
         map (fn f64 (int j) =>
                if (j < 2*i) && (X[j] == j)
                then a[j*i] else 0.0
             ,iota(N))
      ,zip(iota(M), A) )
