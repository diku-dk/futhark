-- ==
-- input { empty([0]i64) } auto output
-- random input { [1]i64 } auto output
-- random input { [256]i64 } auto output
-- random input { [1000]i64 } auto output
-- random input { [1000000]i64 } auto output

-- random input { [1000000000]i64 } auto output

let main [n] (foo: [n]i64): [n]i64 =
  loop foo for _i < 15 do
    scan (+) 0 foo
