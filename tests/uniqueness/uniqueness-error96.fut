-- The condition of a while loop is evaluated before every iteration, so it
-- cannot consume a variable from outside the loop.
-- ==
-- error: Consuming "xs", which is not consumable

def main (xs: *[]i32) =
  loop i = 0 while (let _ = xs with [0] = i in i < 3) do i + 1
