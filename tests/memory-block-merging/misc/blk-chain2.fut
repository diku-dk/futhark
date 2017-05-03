-- Example of Chaining Coalescing inside a block
-- ==
-- input {  0
--          0
--          0
--          [1,2]
--          [[1,2], [3,4]]
--          [ [ [1,2], [3,4] ]
--          , [ [5,6], [7,8] ]
--          ]
--       }
-- output {
--          [ [ [1i32, 2i32], [1i32, 2i32] ]
--          , [ [5i32, 6i32], [7i32, 8i32] ]
--          ]
--        }

-- With the current code, there are only 2 coalescing
-- opportunities, namely `u[i1+i2] = c` and `u[ind1] = b`.
-- This is because the use of `z` in the definition of `ind1`
-- prevents the coalescing of `z[i1+i2+k] = u`.
-- However if `ind1` is computed as `i1+1` (uncomment) then
-- all three coalescing opportunities should be exploited,
-- i.e., including `z[i1+i2+k] = u`.
let main( i1 : i32, i2 : i32, k : i32
        , a : [#n]i32, v : [#m][#n]i32
        , z: *[#n][#m][#n]i32)
        : *[n][m][n]i32 =
  let u       = map (\x -> map (+1) x) v

  ----------------------------------------
  -- Tricky sequence: 'b' should be changed to use the memory block of
  -- 'u[ind1]', but 'ind1' is defined after 'b', so it is not in scope when 'b'
  -- is defined.
  --
  -- FIXME: Disallow memory coalescing in this case, but probably add a pass
  -- before memory block merging that tries to move the 'ind1' definition up.
  -- How should the compiler know which definitions to move up?
  --
  -- NOTE: This is a problem in many of the 'misc' tests.
  let b       = map (+i1) a
  let ind1    = z[k,i1,i2] - i1 -- i1 + 1
  let u[ind1] = b
  ----------------------------------------

  ----------------------------------------
  -- Tricky sequence: Same as before, except both 'i1' and 'i2' *are* defined
  -- before 'c'.  The problem here is that the generated subexpression
  -- containing the result of i1+i2 is put in just before the 'u' line where it
  -- is used, i.e. after the 'c' line.
  --
  -- FIXME: Disallowing the above case also catches this case, but maybe
  -- something smarter can be accomplished?
  let c     = map (+i2) a
  let u[i1+i2] = c
  ----------------------------------------

  let z[i1+i2+k] = u
  in  z
