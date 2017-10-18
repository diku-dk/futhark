-- A better name would be 'take'...
-- ==
-- input { 2 [1,2,3,4,5] }
-- output { [1,2] }
let main(n: i32, a: []i32): []i32 =
  #1 (split (n) a)
