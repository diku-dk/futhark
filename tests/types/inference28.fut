-- An inferred record suddenly has its fields determined.
-- ==
-- input { 2 } output { 2 }

let f r = let y: i32 = r.l
          let _: {l:i32} = r
          in y

let main (l: i32) = f {l}
