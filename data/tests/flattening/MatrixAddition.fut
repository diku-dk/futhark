fun [int] addRows ([int] xs, [int] ys) =
  map(+, zip (xs,ys))

fun [[int]] addMatricies ([[int]] A, [[int]] B) =
  map (addRows, zip (A,B))

fun [[int]] main([[int]] A, [[int]] B) =
  addMatricies(A,B)
