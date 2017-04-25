-- A parametric type can be instantiated with an array.
-- ==
-- input { [[1],[2],[3]] } output { [[3],[2],[1]] }

let reverse [n] 't (a: [n]t): [n]t = a[::-1]

let main (x: [][]i32) = reverse x
