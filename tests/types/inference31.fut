type img 'a = f32 -> a

let f r: (img bool -> img bool) =
  \reg -> let f d x = reg (d+x:f32)
          in f r
