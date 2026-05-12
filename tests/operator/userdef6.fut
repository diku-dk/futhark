-- Polymorphic infix operators ought to work, even in sections.
-- ==
-- input { [[1],[2],[3]] [[4],[5],[6]] [[true]] [[false]] }
-- output { [[1],[2],[3]] [[true]]
--          [[1],[2],[3]] [[true]] }

def (++) 't (xs: []t) (ys: []t) = xs

def main (xss: [][]i32) (yss: [][]i32) (ass: [][]bool) (bss: [][]bool) =
  ( map2 (++) xss yss
  , map2 (++) ass bss
  , map (++ [1]) xss
  , map ([true] ++) bss
  )
