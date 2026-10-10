-- The condition of a while loop cannot consume the loop parameter, which is
-- the result of the loop once the condition is false.
-- ==
-- error: Consuming "xs", which is not consumable

def main (xs0: *[]i32) =
  loop (xs: *[]i32) = xs0 while (let xs[0] = 1 in xs[1] < 10) do
    xs with [1] = xs[1] + 1
