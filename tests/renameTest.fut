-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }
let f(x: (i32,i32), y: i32, z: i32): (i32,i32) = x

let main(): (i32,i32) =
    let x = 1 + 2
    let x = (x + 5, 4+7)
    let (x, (y,z)) = (x, x) in
        f(x,y,z)
