-- ==
-- random input { 2 10 } output { [0,10] }
-- random input { 2 1000 } output { [0,1000] }
-- random input { 0 2 } output { empty(i32) }
-- random input { 0 1000 } output { empty(i32) }

let array n m = map (\i -> replicate m i) (iota n)

entry main n m: []i32 = array n m |> map i32.sum
