-- An application of "trap" that supplies every argument of its type is
-- refined by parametricity: each such call computes its own "r", so its
-- result is fresh.
-- ==
-- input { 7 } output { [5,7,7] [7,7,7] }

def trap 'a 'b 'c (f: a -> b) (x: a) : c -> b =
  let r = f x
  in \(_: c) -> r

def mk_new (x: i32) : *[3]i32 = replicate 3 x

def main (x: i32) =
  let a = trap mk_new x 1i32
  let b = trap mk_new x 2i32
  let a[0] = 5
  in (a, b)
