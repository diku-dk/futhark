-- Very Simple Example of Loop Coalescing.
-- ==
-- input {  [ [1, 2], [0, 0] ]
--          [1, 2]
--       }
-- output {
--          [ [1, 9], [3, 4] ]
--        }

-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

import "/futlib/array"

-- FIXME: The coalescing into y[n/2] causes problems for the loop: In the first
-- iteration it uses a_mem -- which has no special index function -- but in the
-- second iteration, if there is a coalescing, it uses y_mem with an index
-- function created from the `[n/2]` slice.
--
-- The previous way to fix this was to set the initial value of a1 to 'copy a'
-- instead of 'a', and then let both the 'copy a' memory block AND
-- double_buffer_mem be coalesced into y[n/2].  This way, the index function of
-- the existential memory block can be kept the same for all its aliased memory
-- blocks.  However, it would necessitate a copy that should not be necessary.
--
-- A better solution is maybe to extend the DoLoop context merge parameters with
-- index functions, so that each iteration can use different index functions.
--
-- Right now it just gives a wrong result...

-- Code below should result in 2 successful coalescing
-- operations in the memory block of `y`. These are:
-- (i) @y[n/2] = a1@
-- (ii) `a1 = implicit-copy(x2)` (already optimised away
-- because the result of the body is a copy, so the compiler
-- actually only needs to do 1 coalescing: (i)).
--
-- Note that the coalescing of `x1` in `x2 = copy x1`
-- should FAIL, because its computation requires `a1`,
-- hence `x1` cannot be coalesced in the memory block
-- of `y`, i.e., getting 2 successful coalescing
-- operations would be incorrect! This last memory reuse
-- can potentially be done by linear-scan register
-- allocation later!
let main [n] [m] (y: *[n][m]i32, a: [m]i32): *[n][m]i32 =
  let y[0,1] = 9
  let a1 = loop a1 = a for _i < n do
    let x1 = map (+1) a1
    let x2 = copy x1
    in x1

  let y[n/2] = a1
  in  y
