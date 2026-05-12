-- VJP form of #2229.
-- ==
-- tags { autodiff }
-- input { 1.0 5.0 }
-- output { -2.0 }

def grad f x = vjp f x 1f64

def inner (a: f64) (b: f64) =
  let p x = (x - b) ** 2
  in grad p a

entry main (a: f64) (b: f64) =
  grad (inner a) b
