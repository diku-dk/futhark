-- ==
-- input  { [[[1,2], [3,4]], [[2,1], [4,3]]] }
-- output { [[[1,3], [2,4]], [[2,4], [1,3]]] }

def main (xsss: [][][]i32) : *[][][]i32 = map transpose xsss
