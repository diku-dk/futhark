-- Currently fails; an array that is too small is produced somehow.  I
-- suspect replication.
-- ==
-- input { [[[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]], [[12, 13, 14, 15], [16, 17, 18, 19], [20, 21, 22, 23]]] }
-- auto output

def dup = replicate 5 >-> transpose >-> flatten

def main (z: [2][3][4]i32) = z |> map (map dup) |> map dup |> dup
