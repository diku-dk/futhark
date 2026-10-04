def f 'a (A: []([]a, []a)) = A[0].1

entry main (A: *[]([](i32, i32), [](i32, i32))) =
  f A with [0] = (0, 0)
