-- | If a custom derivative occurs at top level, just get rid of it.

-- ==
-- tags { autodiff }
-- entry: do_primal
-- input { 2.5 } output { 5.0 }

-- ==
-- entry: do_vjp
-- compiled input { 2.5 } output { 3.23606797749979 }

def primal (x: f64) =
  with_vjp (\x -> x * 2)
           (\c x_adj -> x_adj + f64.sqrt c)
           x

entry do_vjp x = vjp primal x 1
entry do_primal x = primal x
