
def szs (n: {i64 | \x -> Range x (0,inf)}) : {[]i64 | \y -> Range y (0,10)} =
  map (\_i -> 10) (iota n)

