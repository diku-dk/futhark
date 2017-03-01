-- Record pattern in function parameter.
-- ==
-- input { 2 } output { 1 3 }

fun f {a:i32, b=c:i32} = (a,c)

-- And with a little fancier ascription.
type t = {c: i32, d: i32}
fun  g ({c,d}:t) = f {a=c,b=d}

fun main(x: i32) =
  g {c=x-1,d=x+1}
