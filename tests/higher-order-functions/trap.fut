-- A partial application is not refined by parametricity: "k" holds the "r"
-- that "trap" computed, and every call of "k" returns it, so the results of
-- two calls must be considered aliased.
-- ==
-- error: consumed

def trap 'a 'b 'c (f: a -> b) (x: a) : c -> b =
  let r = f x
  in \(_: c) -> r

def mk_new (x: i32) : *[3]i32 = replicate 3 x

def main (x: i32) =
  let k = trap mk_new x
  let a = k 1i32
  let b = k 2i32
  let a[0] = 5
  in (a, b)
