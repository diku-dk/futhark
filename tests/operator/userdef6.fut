-- Polymorphic infix operators ought to work, even in sections.
-- ==
-- input { [[1],[2],[3]] [[4],[5],[6]] [[true]] [[false]] }
-- output { [[1,4],[2,5],[3,6]] [[true,false]]
--          [[1,1],[2,1],[3,1]] [[true,false]] }

let (++) 't (xs: []t) (ys: []t) = concat xs ys

let main (xss: [][]i32) (yss: [][]i32) (ass: [][]bool) (bss: [][]bool) =
  (map2 (++) xss yss, map2 (++) ass bss,
   map (++[1]) xss, map ([true]++) bss)
