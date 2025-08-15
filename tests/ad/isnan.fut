-- We should not crash on functions like isnan (or isinf for that
-- matter) that have a differentiable domain, but a nondifferentiable
-- codomain.

-- ==
-- tags { autodiff }

entry fwd = jvp f32.isnan
entry rev = vjp f32.isnan
