-- Careful; this one cannot be sunk.
-- ==
-- structure distributed { /Index 5 }

let main (arr: *[](i32, i32, i32, i32, i32)) =
  let (a,b,c,d,e) = arr[0]
  let arr' = rotate 1 arr
  let arr'[1] = (0,0,0,0,0)
  in if a == 0 then arr' else
     let v = a + b + c + d + e
     in arr' with [0] = (v,v,v,v,v)
