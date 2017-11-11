-- Record field access can be nested.
-- ==
-- input { 2 } output { 3 }

let main(x: i32) =
  let r = { {a=x,b=x+1,c={a=x,{a={b=x+1}}}} }
  in r.c.a.b
