-- An element of the array a for-in loop iterates over aliases that array, so
-- returning it for a consumed loop parameter is not fresh.
-- ==
-- error: Return value for consuming loop parameter "acc" aliases "xss"

def main (xss: [][3]i32) =
  loop acc = replicate 3 0 for xs in xss do
    let acc[0] = 42
    in xs
