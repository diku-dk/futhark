-- ==
-- input { [[[1,2],[4,5],[7,8]],[[8,4],[5,1],[7,2]]] }
-- output { [5,1] }

def main (xsss: [][][]i32) = map (\xs -> (opaque (transpose (opaque xs)))[1,1]) xsss
