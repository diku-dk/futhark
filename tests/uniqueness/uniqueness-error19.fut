-- Test that you cannot consume free variables in a loop.
-- ==
-- error:

let main(): i32 =
  let n = 10
  let a = iota(n)
  let b = iota(n) in
  loop (b) = for i < n do
               let a[i] = i in -- Error, because a is free and
                               -- should not be consumed.
               b in
  0
