-- Record pattern in function parameter.
-- ==
-- input { 2 } output { 1 3 }

let f {a:i32, b=c:i32} = (a,c)

-- And with a little fancier ascription.
type t = {c: i32, d: i32}
let  g ({c,d}:t) = f {a=c,b=d}

let main(x: i32) =
  g {c=x-1,d=x+1}
