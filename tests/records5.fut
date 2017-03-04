-- Record construction is right-biased.
-- ==
-- input { 2 } output { 2 1 }

fun main(x: i32) =
  let r = {b=x-1, a=x+1}
  let r' = {r, a=x}
  in (#a r', #b r')
