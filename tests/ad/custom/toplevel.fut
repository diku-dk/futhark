-- | If a custom derivative occurs at top level, just get rid of it.

-- ==
-- tags { autodiff }
-- entry: do_primal
-- input { 2.5f32 } output { 5.0f32 }

-- ==
-- entry: do_vjp
-- compiled input { 2.5f32 } output { 3.23606797749979f32 }

def primal (x: f32) =
  with_vjp (\x -> x * 2)
           (\c x_adj -> x_adj + f32.sqrt c)
           x

entry do_vjp x = vjp primal x 1
entry do_primal x = primal x
