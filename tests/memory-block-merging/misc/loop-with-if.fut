default (f32)

let main (n: i32, xs: []f32, ys: []f32, cond: bool): []f32 =
  loop (xs) = for _i < n do
    if cond
    then xs
    else ys
  in xs
