-- Size-lifted types can be returned from 'if'.

def f '~a (b: bool) (x: a) (y: a) =
  if b then x else y
