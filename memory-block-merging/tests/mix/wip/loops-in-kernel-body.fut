-- This is just to demonstrate that the compiler also performs its memory
-- optimisations inside kernel bodies.  However, this test program is wrong,
-- since the reduce operator is not associative.  There is probably a better way
-- to construct a body like this whose structure will not have been optimised
-- away beyond recognition before reaching the memory block pass in the
-- compiler.  FIXME.
-- ==

-- input {  }
-- output {  }
-- structure cpu { Alloc  }
-- structure gpu { Alloc  }


let reducer (x: [#n]i32) (y: [#n]i32): [n]i32 =
  let z = map (+) x y
  let v = map (+ z[0]) x
  let w = map (+ v[0]) y
  in w

let main (xs: [#n][#n]i32): [n]i32 =
  let initial = replicate n 0
  in reduce reducer initial xs
