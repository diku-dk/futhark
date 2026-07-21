-- Recursion with a declared return type.
-- ==
-- tags { disable }
-- input { [ 5,4,3,2,1 ] }
-- output { [ 1,2,3,4,5] }

def par 'a 'b 'c 'd (f: a -> ?[n].[n]c) (g: b -> ?[m].[m]c) (a: a, b: b) : ?[n][m].([n]c, [m]c) =
  (f a, g b)

def quicksort [n] (xs: [n]i32) : [n]i32 =
  sized n (if n <= 1
           then xs
           else let pivot = xs[0]
                let (a, b) = partition (<= pivot) (drop 1 xs)
                let (a', b') = par quicksort quicksort (a, b)
                in a' ++ [pivot] ++ b')

entry main = quicksort
