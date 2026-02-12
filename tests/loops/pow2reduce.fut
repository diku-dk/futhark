-- Tree reduction that only works on input that is a power of two.
-- ==
-- tags { no_webgpu }
-- input { [1,2,3,4] }
-- output { 10 }

def step [k] (xs: [2 ** k]i32) : [2 ** (k - 1)]i32 =
  tabulate (2 ** (k - 1)) (\i -> xs[i * 2] + xs[i * 2 + 1])

def sum [k] (xs: [2 ** k]i32) : i32 =
  head (loop xs for i in reverse (iota k) do
          step (xs :> [2 ** (i + 1)]i32))

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

def main [n] (xs: [n]i32) =
  let k = ilog2 n
  in sum (xs :> [2 ** k]i32)
