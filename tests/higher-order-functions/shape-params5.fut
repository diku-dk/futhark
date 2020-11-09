type^ nn 'u = { f : u }

let connect '^u (a: nn u) (b: nn u) : nn (u, u) =
  { f = (a.f, a.f)
  }

let nn1 : nn ((n: i64) -> [n]i32 -> [n]i32) =
  { f = \n (xs: [n]i32) -> xs
  }

let foo = connect nn1 nn1

let main [n] (xs: [n]i32) = foo.f.0 n xs
