-- Test that you can shadow a function with a variable.
--
-- ==
-- error: f
def f (x: i32) : i32 = x + 2

def main (x: i32) : i32 =
  let f = 3
  in f x
