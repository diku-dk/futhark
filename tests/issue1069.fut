-- The 'unsafe' is just to make the code a little neater.  It does not
-- affect the bug.

def main xs =
  #[unsafe]
  tabulate_2d 10 10 (\a i ->
                       let x = xs[a + i]
                       in loop xs = xs for j < 10 do map (+ x) xs)
