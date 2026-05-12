type^ nn 'u = {f: u}

def connect '^u (a: nn u) (b: nn u) : nn (u, u) =
  { f = (a.f, a.f)
  }

def nn1 : nn ((n: i64) -> [n]i32 -> [n]i32) =
  { f = \n (xs: [n]i32) -> xs
  }

def foo = connect nn1 nn1

def main [n] (xs: [n]i32) = foo.f.0 n xs
