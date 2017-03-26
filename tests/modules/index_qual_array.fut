-- ==
-- input { 4 } output { 5 }

module M = {
  let a: []i32 = [1,2,3]
}

fun main(x: i32): i32 = M.a[0] + x
