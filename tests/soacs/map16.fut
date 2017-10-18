-- Map returning an array predicated on the index variable.
--
-- ==
-- input { 2 }
-- output { [[0], [1]] }

let main(chunk: i32): [][]i32 =
  map (\(k: i32): [1]i32  ->
         if k==0 then [0] else [1]
     ) (iota(chunk))
