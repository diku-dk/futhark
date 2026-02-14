-- ==
-- input { [[1,2,3],[4,5,6]] }
-- auto output

def dup = replicate 2 >-> transpose >-> flatten

entry main (z: [][]i32) = z |> map dup |> dup
