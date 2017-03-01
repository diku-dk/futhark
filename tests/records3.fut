-- Test tuple patterns.
-- ==
-- input { 2 } output { 3 1 }

fun f(x: i32) = {a=x+1,b=x-1}

fun main(x: i32) =
  let {a, b=c} = f x
  in (a,c)
