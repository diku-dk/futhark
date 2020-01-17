type^ img 'a = f32 -> a

let f r: (img bool -> img bool) =
  \reg -> let g d x = reg (d+x:f32)
          in g r
