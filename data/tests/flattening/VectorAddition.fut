fun [int] main([int] A, [int] B) =
  map(op+, zip (A,B))
