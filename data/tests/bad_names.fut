-- Tests that the code generator does not choke on terrible names that
-- are not valid in C.
--
-- ==
-- input { False 2 }
-- output { 12 }

fun int f(bool r, int x) = x+1 + if r then f(r,x) else 0
fun int f'(bool r, int x) = x+2 + if r then f'(r,x) else 0
fun int f_(bool r, int x) = x+3 + if r then f_(r,x) else 0

fun int main(bool r, int x) =
  let x'_ = x + 1 in
  let x'' = x'_ + x'_ in
  f_(r, f'(r, f(r, x'')))
