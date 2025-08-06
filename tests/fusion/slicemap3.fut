-- ==
-- input { [[1,2,3],[4,5,6],[7,8,9]] } output { [[9, 15], [27, 33]] }
-- structure gpu { Index 1 }

def main (xs: [][]i32) = map (map (* 3)) ((map (map (+ 2)) xs)[::2, ::2])
