-- An element of the array a for-in loop iterates over aliases that array, so
-- returning it is not fresh.
-- ==
-- error: "xss", which is not consumable

def f (xss: [][3]i32) : *[3]i32 =
  loop acc = replicate 3 0 for xs in xss do xs

def main (xss: [][3]i32) =
  let r = f xss
  let r[0] = 42
  in (r, xss)
