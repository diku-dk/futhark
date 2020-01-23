-- Size-lifted types can be returned from 'if'.

let f '~a (b: bool) (x: a) (y: a) =
  if b then x else y
