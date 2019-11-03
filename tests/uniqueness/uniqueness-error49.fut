-- Do not let ascription screw up uniqueness/aliasing.
-- ==
-- error: Attempt to consume variable `xs`

let f 't (x: t) = id (x : t)
let main (xs: []i32) = f xs with [0] = 0
