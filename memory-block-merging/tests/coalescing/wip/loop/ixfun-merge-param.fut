-- Problem with lacking index function merge parameters in loops.
-- ==
-- input { [1, 2] }
-- output { [1, 2, 3, 4] }

-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

-- FIXME: The coalescing into x[0] causes problems for the loop: In the first
-- iteration it uses a_mem -- which has no special index function -- but in the
-- second iteration, if there is a coalescing, it uses x_mem with an index
-- function created from the `[0]` slice.  Currently this is disabled, but a
-- better solution is maybe to extend the DoLoop context merge parameters with
-- index functions, so that each iteration can use different index functions.
-- Currently this case is just disabled.
--
-- Also see loop-0.fut.

let main [n] (a0: [n]i32): []i32 =
  let a1 = loop b0 = a0 for _i < n do
    let b1 = map (+ 1) b0
    -- Details: See ixfun-analysis-across-iterations.fut.
    in b1
  let x = concat a0 a1
  in x
