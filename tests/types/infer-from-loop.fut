-- When generalising 'repeat', infer that the type of 'x' must not be a function.

def repeat f n x = loop x for i < n do f x

entry main (xs: []i32) =
  let ys = repeat (map (+ 1)) 10 xs
  in ys
