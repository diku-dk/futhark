-- Size for something that is partially applied.

let f [n] (x: [n]f32): [n]f32 = x

let main : []f32 -> []f32 = f
