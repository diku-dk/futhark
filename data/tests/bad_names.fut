-- Tests that the code generator does not choke on terrible names that
-- are not valid in C.
--
-- ==
-- input { 2 }
-- output { 6 }

fun int main(int x) =
  let x'_ = x + 1 in
  let x'' = x'_ + x'_ in
  x''
