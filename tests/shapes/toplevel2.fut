-- #1993, do not allow size-polymorphic non-function bindings with
-- unknown sizes.
-- ==
-- error: size-polymorphic value binding

def foo [n] = (iota n, filter (> 5) (iota n))
