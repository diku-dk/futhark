-- Does iota work at all?
-- ==
-- input { 0 }
-- output { empty(i32) }
-- input { 2 }
-- output { [0,1] }

let main(n: i32): []i32 = iota(n)
