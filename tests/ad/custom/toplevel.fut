-- | If a custom derivative occurs at top level, just get rid of it.

-- ==
-- entry: do_primal
-- input { 2.5 } output { 5.0 }

-- ==
-- entry: do_vjp
-- compiled input { 2.5 } output { 2.58113883008419 }

def primal (x: f64) =
  vjp_by (\x -> (x * 2, x))
         (\(x_adj, c) -> x_adj + f64.sqrt c)
         x

entry do_vjp x = vjp primal x 1
entry do_primal x = primal x
