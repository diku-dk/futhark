-- Can we match a module with an unnamed signature?
-- ==
-- input { 5 } output { 7 }

module M: {val x: i32} = {
  let x: i32 = 2
  let y: i32 = 3
}

let main(x: i32) = M.x + x
