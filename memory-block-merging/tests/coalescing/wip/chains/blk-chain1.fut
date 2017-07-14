-- Test1 Memory-Block Merging
--
-- FIXME: The coalescing seems to work okay, but it cannot handle that x and b
-- are previously fused into a single map, in which the b body is evaluated
-- *first* (which normally would not matter, but is important in this
-- ExplicitMemory representation).  The compiler rightly stops the `x[2] = b`
-- coalescing from happening, since otherwise the body of x would overwrite the
-- entire result of b.  Even if they are swapped back in the right order, the
-- compiler would not recognize that it is legal.  This is very much an edge
-- case.
--
-- It is easy enough to not let it happen in this specific case: Just comment
-- out the code below to restrict the fusion to see for yourself.
-- ==
-- input { [0, 3, 5, 7, 9] }
-- output { [[0, 0, 0, 0, 0],
--           [6, 6, 6, 6, 6],
--           [10, 10, 10, 10, 10],
--           [14, 14, 14, 14, 14],
--           [18, 18, 18, 18, 18],
--           [0, 0, 0, 0, 0],
--           [3, 3, 3, 3, 3],
--           [1, 4, 6, 8, 10],
--           [7, 7, 7, 7, 7],
--           [9, 9, 9, 9, 9]]
--        }
-- structure cpu { Alloc 2 }

-- Without allocation hoisting, there is only one coalescing opportunity, i.e.
-- `x[2] = b`.
--
-- The other one, i.e. `y = concat a2 x`, only succeeds if the allocation of `y`
-- is hoisted to before the creation of `x` and `a2`.

let main (a: [#n]i32): [][n]i32 =
  let x    = map (\i -> replicate n i) a
  -- let k = x[0, 0] -- uncomment to restrict fusion; also replace '1' with 'k'
  let b    = map (+ 1) a -- Will be merged.
  let x[2] = b
  let a2   = map (\i -> replicate n (2 * i)) a -- Will be merged.
  let y    = concat a2 x
  in  y
