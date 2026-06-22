-- Tricky because clz has a different return type than input type.
-- Not really differentiable, though.
-- ==
-- tags { no_webgpu }

entry vjp_u64 = vjp u64.clz
entry jvp_u64 = jvp u64.clz
