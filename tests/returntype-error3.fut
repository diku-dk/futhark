-- This test demonstrates a limitation caused by the conservativity of
-- the aliasing analyser.
-- ==
-- error:

-- The two arrays must not alias each other, because they are unique.
def main () : (*[]i32, *[]i32) =
  let n = 10
  let a = iota (n)
  in if 1 == 2 then (a, iota (n)) else (iota (n), a)

-- The type checker decides that both components of the tuple may
-- alias a, so we get an error.
