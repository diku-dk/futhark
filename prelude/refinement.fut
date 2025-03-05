import "soacs"
import "array"

def and (_: []bool) : bool = ???

def injectiveRCD 't (_ : (i64, i64)) (_: []t) : bool = ???

def bijectiveRCD 't (_ : (i64, i64)) (_ : (i64, i64)) (_: []t) : bool = ???

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 (c: bool): i64 = if c then 1 else 0

def all (p: i64 -> bool) (n: i64) =
  and (map (\i -> p i) (iota n))

-- def partitionedInv [n] (is: [n] i64) (p: i64 -> bool) = ???
-- def filtPartInv [n] (X : [n]i64) (filt: [n]bool) (part: [n]bool) (s: i64) =
def filtPartInv [n] (_X : [n]i64) (_filt: i64 -> bool) (_part: i64 -> bool) = ???
  -- let m = sum (map (\c -> to_i64 c) filt)
  -- let step1 = bijectiveRCD (0, m-1) (0, m-1) X
  -- let step2 = all (\i -> if filt[i] then true else X[i] < 0 || X[i] >= m) n
  -- let step3 = true -- needs i < j
  -- let step4 = true -- needs i < j
  -- let step5 = all (\i -> if part[i] then X[i] < s else X[i] >= s) m
  -- in step1 && step2 && step5
