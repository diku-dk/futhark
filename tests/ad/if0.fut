-- ==
-- tags {disable}

let f (b, x, y) =
  if b then x*y else x/y

entry f_jvp b x y =
  (jvp f (b,x,y) (b,1,0),
   jvp f (b,x,y) (b,0,1))

entry f_vjp b x y =
  vjp f (b,x,y) 1
