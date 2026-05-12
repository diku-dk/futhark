-- ==
-- input { 2 2 [1,2,3,4] }
-- output { [[1,3],[2,4]] }

def unpack_2d 't n m : [i64.i32 n * i64.i32 m]t -> [][]t = unflatten

def main n m xs : [][]i32 = unpack_2d n m xs |> transpose
