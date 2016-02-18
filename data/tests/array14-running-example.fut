-- Example program from the ARRAY'14 paper.
-- ==
-- tags { no_opencl }

fun [[real]] main(*[int] X, *[[real]] A) =
  let M = size(0,A) in
  let N = size(1,A) in
  map (fn *[real] ({int, *[real]} e) =>
         let {i, a} = e in
         loop(a) = for j < N do
           let a[j] = a[ X[j] ] * 2.0 in a
         in
         map (fn real (int j) =>
                if (j < 2*i) && (X[j] == j)
                then a[j*i] else 0.0
             ,iota(N))
      ,zip(iota(M), A) )
