-- ==
-- random input { 2 10 } output { [0,10] }
-- random input { 2 1000 } output { [0,1000] }
-- random input { 0 2 } output { empty([0]i32) }
-- random input { 0 1000 } output { empty([0]i32) }
-- random input { 1000 2 } auto output
-- random input { 1000 0 } auto output

let array n m = map (\i -> replicate m i) (iota n)

entry main n m: []i32 = array n m |> map i32.sum
