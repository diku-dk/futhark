-- ==
-- random input { 2i64 10i64 } output { [0,10] }
-- random input { 2i64 1000i64 } output { [0,1000] }
-- random input { 0i64 2i64 } output { empty([0]i32) }
-- random input { 0i64 1000i64 } output { empty([0]i32) }
-- random input { 1000i64 2i64 } auto output
-- random input { 1000i64 0i64 } auto output

def array n m = map (\i -> replicate m (i32.i64 i)) (iota n)

entry main n m : []i32 = array n m |> map i32.sum
