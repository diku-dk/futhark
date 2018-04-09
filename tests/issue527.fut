-- ==
-- input { 2 } output { 2 }

let id 'a (x: a) : a = x
let main (x: i32) = let r = { id }
                    in r.id x
