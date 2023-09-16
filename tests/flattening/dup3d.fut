-- Currently fails; an array that is too small is produced somehow.  I
-- suspect replication.
-- ==
-- input { [[[1,2],[3,4]],[[5,6],[7,8]]] }
-- auto output

def dup = replicate 2 >-> transpose >-> flatten

def main (z: [2][2][2]i32) = z |> map (map dup) |> map dup |> dup
