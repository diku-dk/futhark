-- ==
-- error:
fun int f(([int],*[int]) t) =
  let (a,b) = t in
  let b[0] = 1337 in
  a[0]

fun int main(*[int] b) =
  let a = b in
  -- Should fail, because 'a' and 'b' are aliased, yet the 'b' part of
  -- the tuple is consumed.
  f((a,b))
