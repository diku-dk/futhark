-- Do records work at all?
-- ==
-- input { 2 } output { 1 3 }

let f(x: i32) = {y=x+1,x=x-1}

let main(x: i32) =
  let r = f x
  in (#x r, #y r)
