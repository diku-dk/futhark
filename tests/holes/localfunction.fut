-- Based on #2488

def foo =
  let f '^a '^b (_: a): b = ???
  in f true false
