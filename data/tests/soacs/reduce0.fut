-- How quickly can we reduce arrays?
--
-- ==
-- input { 0 }
-- output { 0 }
-- input { 100 }
-- output { 4950 }
-- compiled input { 100000 }
-- output { 704982704 }
-- compiled input { 100000000 }
-- output { 887459712 }

fun int main(int n) =
  reduce(+, 0, iota(n))
