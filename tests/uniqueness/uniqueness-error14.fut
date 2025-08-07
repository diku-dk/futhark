-- This program tests whether the compiler catches some of the more
-- nasty side cases of aliasing in loops.
-- ==
-- error: "arr" aliases "barr"

def main () : i64 =
  let arr = copy (iota (10))
  let barr = copy (iota (10))
  let arr =
    loop arr for i < 10 do
      let arr[i] = 0
      -- Consume arr and its aliases...
      in barr
  -- Because of this, arr should be aliased to barr.
  in barr[0]

-- Error, barr has been consumed!
