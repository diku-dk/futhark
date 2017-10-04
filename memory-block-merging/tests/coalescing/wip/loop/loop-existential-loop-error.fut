-- Same as loop-existential-loop.fut, except here the inner loop memory must not
-- be coalesced with the result memory of the outer loop.
-- ==
-- input { [3, 6]
--         2
--       }
-- output { [5, 5]
--        }

-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 2 }

import "/futlib/array"

let main [n] (a0: [n]i32, n_iter: i32): []i32 =
  let a2 = loop a = a0 for _i < n_iter do
    let inner_loop_mem = replicate n 0
    let a' = loop mem = inner_loop_mem for j < n do
      -- If inner_loop_mem is coalesced into double_buffer_mem, we get this
      -- problem:
      --
      -- When _i == 1: a == double_buffer_mem == inner_loop_mem == mem
      -- When j == 0: let mem[0] = a[0] + 1
      -- When j == 1: let mem[1] = mem[0] + 1
      --
      -- What happens is that when j == 0 it gets the right value from a[0], but
      -- then it overwrites it with the result + 1.  When j == 1 it then gets
      -- the newest value at index 0 in the merged memory block, which in this
      -- case is the value a[0] + 1.
      --
      -- This is not a problem in loop-existential-loop.fut, since that uses the
      -- pattern `let mem[j] = a[j]`, so it never writes over a value that needs
      -- to be used in a later iteration.
      --
      -- This is also a problem for the reuse pass, so it would be nice to have
      -- something that recognizes the acceptable bodies and ignores the bad
      -- ones.
      let mem[j] = a[0] + 1
      in mem
    in a'
  let a3 = copy a2
  in a3
