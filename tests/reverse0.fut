-- Reverse an array using indexing.
--
-- ==
-- input { [1,2,3,4] }  output { [4,3,2,1] }
-- input { empty([0]i32) } output { empty([0]i32) }
-- structure { Assert 0 }

def main (as: []i32) : []i32 = as[::-1]
