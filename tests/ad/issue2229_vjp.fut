-- VJP form of #2229.
-- ==
-- tags { autodiff }
-- input { 1.0f32 5.0f32 }
-- output { -2.0f32 }

def grad f x = vjp f x 1f32

def inner (a: f32) (b: f32) =
  let p x = (x - b) ** 2
  in grad p a

entry main (a: f32) (b: f32) =
  grad (inner a) b
