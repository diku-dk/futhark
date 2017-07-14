-- Do ranges work at all?
-- ==
-- input { 10 }
-- output { [1,10,19,30] }
-- input { 1 }
-- output { [30] }
-- input { 30 }
-- output { [10] }

let main(n: i32): []i32 =
    concat (concat [10..n...20] [1..n..<20]) [30..n..>0]
