-- ==
-- input { 0 }
-- output { 1i64 2i64 }

let delaylength [x] (arr: [x]i32) (y: i64) = length arr

let main x =
  let f = delaylength [x]
  let g = delaylength [x,x]
  let (f', g') = id (f, g)
  in (f' 1, g' 2)
