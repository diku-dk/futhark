-- Test that we do not hoist dangerous things out of loops.
-- ==
-- input { empty(i32) 2 } output { 2 }

let main(a: [#n]i32, m: i32) =
  loop (x=m) for i < n do
    x+a[2]
