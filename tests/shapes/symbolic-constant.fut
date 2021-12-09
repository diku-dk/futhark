-- A symbolic constant in a type abbreviation should be respected.
-- ==
-- input { 2i64 } output { [0i64,1i64] }
-- input { 3i64 } error: cannot match shape of type `m_ints`

def m = 2i64
type m_ints = [m]i64

def main(n: i64) = iota n :> m_ints
