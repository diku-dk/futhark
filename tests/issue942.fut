-- ==
-- input {} output { [0i64] }

type sometype 't = #someval t

def f (size: i64) (_: i32) : sometype ([size]i64) =
  #someval (iota size)

def apply '^a '^b (f: a -> b) (x: a) = f x

def main : [1]i64 =
  match apply (f 1) 0
  case #someval x -> x
